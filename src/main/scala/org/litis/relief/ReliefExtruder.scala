package org.litis.relief


/** A launcher for the relief extruder app. */
object ReliefExtruder extends App {

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

	var output:String = null
	var input:String = null

//	options(args.drop(1).toList)
	options(args.toList)
	
	def options(list:List[String]) {
		list match {
			case Nil => {}
			case "-box" :: a :: b :: c :: d :: tail => { startx = a.toInt; endx = b.toInt; starty = c.toInt; endy = d.toInt; options(tail) }
			case "-out" :: o :: tail => { output = o; options(tail) }
			case "-volume" :: v :: tail => { volume = v.toDouble; options(tail) }
			case "-scale" :: s :: tail => { scale = s.toDouble; options(tail) }
			case "-yscale" :: y :: tail => { yscale = y.toDouble; options(tail) }
			case "-imagescale" :: min :: max :: tail => { imin = min.toDouble; imax = max.toDouble; options(tail) }
			case "-cellsize" :: s :: tail => { cellsize = s.toDouble; options(tail) }
			case a :: tail => { if(a.startsWith("-")) usage("Unknown option '%s'".format(a)) else { input = a; options(tail) } }
		}
	}

	if(input eq null)
		 usage("You must give an input file name")
	else run()

	def usage(message:String) {
		if(message ne null)
			printf("%s%n%n".format(message))
		println("Usage: relief <input.csv> [-out <output>] [-box <startx endx starty endy>] [-volume <height>] [-scale <factor>] [-scale <factor>]")
		println("       <input.csv> or <input.png>      Either a file in CSV format, where the separator is ; in the specific MNT format,")
		println("                                       or a PNG image prepared in a specific format where each level is a color in the")
		println("                                       chromatic circle (HSV color model). See the -imagescale option to specify minimum (violet)")
		println("                                       and maximum elevations in the image (red). Values are assumed to be meters.")
		println("       -out <output>                   Name of the resulting output file, the '.stl' extension is added if needed")
		println("                                       If given the output is a binary STL file. If not given, output goes to")
		println("                                       the standard output and the STL is in ASCII.")
		println("       -box <startx endx starty endy>  Optionnaly a bounding box to extract a part of the file. Values are row/column indices.")
		println("       -volume <height>                The height of the base added to the surface, if not given, only the surface")
		println("                                       is output. Value is assumed to be meters.")
		println("       -scale <factor>                 Allow to scale the whole model by a given factor.")
		println("       -yscale <factor>                Allow to scale only the elevation (may be used with -scale).")
		println("       -imagescale <min> <max>         Specify the minimum <min> and maximum <max> elevations in an input image (not")
		println("                                       used in csv format). Values are assumed to be meters.")
		println("       -cellsize <size>                Spacing between data points. Values are assumed to be meters.")
		sys.exit(1)
	}
	
// Run

	def run() {
		val outFile = if(output ne null) (if(output.endsWith(".stl")) output else "%s.stl".format(output)) else null

		print("* Reading       ")
		val heightMap = HeightMap(input, startx, endx, starty, endy, scale, yscale, imin, imax, cellsize)
		heightMap.setVolume(volume)
		println(" OK")
		
		print("* Normalizing   ")
		heightMap.normalize()
		println("OK")

		print("* Triangulating [%d triangles]".format(heightMap.triangleCount))
		heightMap.beginSTL("MNT", outFile, (outFile ne null))
		heightMap.triangulate
		heightMap.endSTL
		println(" OK")
	}
}
