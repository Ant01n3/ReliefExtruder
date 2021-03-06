package org.litis.relief

import scala.collection.mutable.ArrayBuffer

object OutputFormat extends Enumeration {
	val STL = Value
	val PNG = Value
	val Unknown = Value

	type OutputFormat = Value
}


/** A launcher for the relief extruder app. */
object ReliefExtruder {

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
	var clamp:(Double,Double) = _
	var cellsize = 1.0
	var format   = OutputFormat.Unknown
	var resize   = 1.0
	var resizeMethod = ResizeMethod.Unknown
	var greyData = false

	var output:String = null
	var input = ArrayBuffer[String]()

	def main(args:Array[String]):Unit = {
//		options(args.drop(1).toList)
		options(args.toList)

		if(input.isEmpty)
			 usage("You must give at least one input file name")
		else input.foreach { run(_) }
	}

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
			case "-clamp" :: min :: max :: tail => { clamp = (min.toDouble, max.toDouble); options(tail) }
			case a :: tail => { if(a.startsWith("-")) usage("Unknown option '%s'".format(a)) else { input += a; options(tail) } }
		}
	}

	def toFormat(f:String):OutputFormat.Value = f.toLowerCase match {
		case "stl" => OutputFormat.STL
		case "png" => OutputFormat.PNG
		case _ => throw new RuntimeException("unknown output format '%s'".format(f))
	}

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
		println("                                       the file is named using the input file name without extension and adding '.png'.")
		println("       -clamp <min> <max>              Clamp the minimum and maximum height of the file. If the heights are withing this bound they")
		println("                                       are unchanged. Else they are reset to the min or max. The min and max heights are also set")
		println("                                       to this value, even if no height in the file reach them. This allows to have several files")
		println("                                       at the same scale.")
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
		println("       -grey                           If an image is passed as input, the hue is used by default to create a height, use grey")
		println("                                       intensity instead with this option. For the PNG output format this options does the")
		println("                                       same for the output.")
		sys.exit(1)
	}
	
// Run

	def run(inFile:String) {
		printf("%s*%s Reading ", Console.YELLOW, Console.RESET)
		var heightMap = HeightMap(inFile, startx, endx, starty, endy, scale, yscale, imin, imax, cellsize, greyData, clamp)
		printf(" %sOK%s%n", Console.GREEN, Console.RESET)
		
		printf("%s*%s Normalizing ", Console.YELLOW, Console.RESET)
		heightMap.normalize()
		printf("[min %f][max %f] ", heightMap.minHeight, heightMap.maxHeight)
		printf("%sOK%s%n", Console.GREEN, Console.RESET)

		if(resize != 1.0 || resizeMethod != ResizeMethod.Unknown) {
			if(resizeMethod == ResizeMethod.Unknown) resizeMethod = Lanczos2
			printf("%s*%s Resize (factor %.2f method %s) ", Console.YELLOW, Console.RESET, resize, resizeMethod)
			heightMap = heightMap.resize(resize, resizeMethod)
			printf("[new hmap %dx%d][min=%f max=%f] ", heightMap.cols, heightMap.rows, heightMap.minHeight, heightMap.maxHeight, clamp)
			printf("%sOK%s%n", Console.GREEN, Console.RESET)

		printf("%s*%s Normalizing ", Console.YELLOW, Console.RESET)
		heightMap.normalize()
		printf("[min %f][max %f] ", heightMap.minHeight, heightMap.maxHeight)
		printf("%sOK%s%n", Console.GREEN, Console.RESET)

		}
		
		heightMap.setVolume(volume)

		if(format == OutputFormat.Unknown) {
			if(output ne null) {
				if(output.toLowerCase.endsWith(".stl")) format = STL
				else if(output.toLowerCase.endsWith(".png")) format = PNG
			} else {
				format = STL
			}
		}

		if(format == STL) {
			val outFile = if(output ne null) (if(output.endsWith(".stl")) output else "%s.stl".format(output)) else null

			printf("%s*%s Triangulating [%d triangles]", Console.YELLOW, Console.RESET, heightMap.triangleCount)
			heightMap.beginSTL("MNT", outFile, (outFile ne null))
			heightMap.triangulate
			heightMap.endSTL
			printf(" %sOK%s%n", Console.GREEN, Console.RESET)
		} else if(format == PNG) {
			val outFile = "%s%s".format(if(output ne null) output else inputWithoutExt(inFile), if((output ne null) && output.endsWith(".png")) "" else ".png")

			printf("%s*%s To PNG", Console.YELLOW, Console.RESET)
			heightMap.toPNG(outFile, greyData)
			printf(" %sOK%s%n", Console.GREEN, Console.RESET)			
		}
	}

	private def inputWithoutExt(in:String):String = in.substring(0, in.lastIndexOf("."))
}
