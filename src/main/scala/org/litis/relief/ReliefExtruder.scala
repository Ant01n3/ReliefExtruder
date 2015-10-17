package org.litis.relief


object OutputFormat extends Enumeration {
	val STL = Value
	val PNG = Value
	val Unknown = Value

	type OutputFormat = Value
}


/** A launcher for the relief extruder app. */
object ReliefExtruder extends App {

	import OutputFormat._
	import ResizeMethod._

// Options

	var startx   = -1
	var endx     = -1
	var starty   = -1
	var endy     = -1
	var volume   = -1.0
	var scale    = 1.0
	var yscale   = 1.0
	var imin     = 0.0
	var imax     = 100.0
	var cellsize = 1.0
	var format   = OutputFormat.Unknown
	var resize   = 1.0
	var resizeMethod = Lanczos2
	var greyData = false

	var output:String = null
	var input:String = null

//	options(args.drop(1).toList)
	options(args.toList)
	
	def options(list:List[String]) {
		list match {
			case Nil => {}
			case "-box" :: a :: b :: c :: d :: tail => { startx = a.toInt; endx = b.toInt; starty = c.toInt; endy = d.toInt; options(tail) }
			case "-out" :: o :: tail => { output = o; options(tail) }
			case "-format" :: f :: tail => { format = toFormat(f); options(tail) }
			case "-resize" :: r :: tail => { resize = r.toDouble; options(tail) }
			case "-resizeMethod" :: m :: tail => { resizeMethod = ResizeMethod.fromString(m); options(tail) }
			case "-volume" :: v :: tail => { volume = v.toDouble; options(tail) }
			case "-scale" :: s :: tail => { scale = s.toDouble; options(tail) }
			case "-yscale" :: y :: tail => { yscale = y.toDouble; options(tail) }
			case "-imagescale" :: min :: max :: tail => { imin = min.toDouble; imax = max.toDouble; options(tail) }
			case "-cellsize" :: s :: tail => { cellsize = s.toDouble; options(tail) }
			case "-grey" :: tail => { greyData = true; options(tail) }
			case a :: tail => { if(a.startsWith("-")) usage("Unknown option '%s'".format(a)) else { input = a; options(tail) } }
		}
	}

	def toFormat(f:String):OutputFormat.Value = f.toLowerCase match {
		case "stl" => OutputFormat.STL
		case "png" => OutputFormat.PNG
		case _ => throw new RuntimeException("unknown output format '%s'".format(f))
	}

	if(input eq null)
		 usage("You must give an input file name")
	else run()

	def usage(message:String) {
		if(message ne null)
			printf("%s%n%n".format(message))
		println("Usage: relief <input.csv> [-out <output>] [-box <startx endx starty endy>] [-volume <height>] [-scale <factor>] [-scale <factor>]")
		println("       <input.csv> or <input.png>      Either a file in CSV/ASC format, where the separator is ; or spaces in the specific ARC info,")
		println("       or <input.asc>                  format or a PNG image prepared in a specific format where each level is a color in the")
		println("                                       chromatic circle (HSV color model). See the -imagescale option to specify minimum (violet)")
		println("                                       and maximum elevations in the image (red). Values are assumed to be meters.")
		println("       -out <output>                   Name of the resulting output file, the '.stl' or '.png' extension is added if needed")
		println("                                       If given and the format is STL, the output is a binary STL file. If not given, output goes to")
		println("                                       the standard output as an ASCII STL. If the format is PNG, and there is no output given")
		println("                                       the file is named  'out.png'.")
		println("       -format <format>                Output format, either 'STL' or 'PNG'. The default format is 'STL'. If the -out option")
		println("                                       has an extension, this one is used and this option is not necessary, but is chosen if given.")
		println("       -resize <factor>                Change the resolution of the heightmap by 'factor' (factor is a real number but not 0).")
		println("       -resizeMethode <method>         One of 'lanczos2', 'lanczos3', 'lanczos4', 'hq3x' or 'hq4x'. Lanczos is an interpolation")
		println("                                       method knonw for giving good results in many situations. Hq3x and Hq4x are magnification")
		println("                                       only methods used to enlarge pixel art. As their name suggests, they enlarge by 3 or 4")
		println("                                       times only and ignore the -resize value.")
		println("       -box <startx endx starty endy>  Optionnaly a bounding box to extract a part of the file. Values are row/column indices.")
		println("       -volume <height>                The height of the base added to the surface, if not given, only the surface")
		println("                                       is output. Value is assumed to be meters.")
		println("       -scale <factor>                 Allow to scale the whole model by a given factor.")
		println("       -yscale <factor>                Allow to scale only the elevation (may be used with -scale).")
		println("       -imagescale <min> <max>         Specify the minimum <min> and maximum <max> elevations in an input image (not")
		println("                                       used in csv format). Values are assumed to be meters.")
		println("       -cellsize <size>                Spacing between data points. Values are assumed to be meters. Needed for images.")
		println("       -grey                           If an image is passed, the hue is used by default to create a height, use grey")
		println("                                       intensity instead with this option.")
		sys.exit(1)
	}
	
// Run

	def run() {
		printf("%s*%s Reading ", Console.YELLOW, Console.RESET)
		var heightMap = HeightMap(input, startx, endx, starty, endy, scale, yscale, imin, imax, cellsize, greyData)
		printf(" %sOK%s%n", Console.GREEN, Console.RESET)
		
		printf("%s*%s Normalizing ", Console.YELLOW, Console.RESET)
		heightMap.normalize()
		printf(" %sOK%s%n", Console.GREEN, Console.RESET)

		if(resize != 1.0 || resizeMethod != ResizeMethod.Unknown) {
			printf("%s*%s Resize (by %.2f)", Console.YELLOW, Console.RESET, resize)
			heightMap = heightMap.resize(resize, resizeMethod)
			printf(" %sOK%s%n", Console.GREEN, Console.RESET)
		}
		
		heightMap.setVolume(volume)

		if(format == OutputFormat.Unknown) {
			if(output.toLowerCase.endsWith(".stl")) format = STL
			else if(output.toLowerCase.endsWith(".png")) format = PNG
			else format = STL
		}

		if(format == STL) {
			val outFile = if(output ne null) (if(output.endsWith(".stl")) output else "%s.stl".format(output)) else null

			printf("%s*%s Triangulating [%d triangles]", Console.YELLOW, Console.RESET, heightMap.triangleCount)
			heightMap.beginSTL("MNT", outFile, (outFile ne null))
			heightMap.triangulate
			heightMap.endSTL
			printf(" %sOK%s%n", Console.GREEN, Console.RESET)
		} else if(format == PNG) {
			val outFile = "%s%s".format(if(output ne null) output else "out", if(output.endsWith(".png")) "" else ".png")

			printf("%s*%s To PNG", Console.YELLOW, Console.RESET)
			heightMap.toPNG(outFile)
			printf(" %sOK%s%n", Console.GREEN, Console.RESET)			
		}
	}
}
