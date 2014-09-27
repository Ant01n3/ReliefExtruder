package org.litis.relief

import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

import org.sofa.math.{Point3, Triangle, ConstTriangle, Rgba}
import org.sofa.nio._

import java.io.{File, InputStream, FileInputStream, FileOutputStream, PrintStream, IOException}
import java.nio.channels.FileChannel
import javax.imageio.ImageIO


object ReliefOld extends App {

// Parsing

	final val NCols     = """ncols;([0-9]+);+""".r
	final val NRows     = """nrows;([0-9]+);+""".r
	final val Yllcorner = """yllcorner;([0-9]+,?[0-9]*);+""".r
	final val Xllcorner = """xllcorner;([0-9]+,?[0-9]*);+""".r
	final val CellSize  = """cellsize;([0-9]+);+""".r
	final val NoData    = """NODATA_value;(-?[0-9]+);+""".r

// Options

	var startx = -1
	var endx   = -1
	var starty = -1
	var endy   = -1
	var volume = -1.0
	var scale  = 1.0
	var yscale = 1.0
	var output:String = null
	var input:String = null

	options(args.drop(1).toList)
	
	def options(list:List[String]) {
		list match {
			case Nil => {}
			case "-box" :: a :: b :: c :: d :: tail => { startx = a.toInt; endx = b.toInt; starty = c.toInt; endy = d.toInt; options(tail) }
			case "-out" :: o :: tail => { output = o; options(tail) }
			case "-volume" :: v :: tail => { volume = v.toDouble; options(tail) }
			case "-scale" :: s :: tail => { scale = s.toDouble; options(tail) }
			case "-yscale" :: y :: tail => { yscale = y.toDouble; options(tail) }
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
		println("       <input.csv>                     A file in CSV format, where the separator is ; in the specific MNT format")
		println("       -out <output>                   Name of the resulting output file, the '.stl' extension is added if needed")
		println("                                       If given the output is a binary STL file. If not given, output goes to")
		println("                                       the standard output and the STL is in ASCII.")
		println("       -box <startx endx starty endy>  Optionnaly a bounding box to extract a part of the file.")
		println("       -volume <height>                The height of the base added to the surface, if not given, only the surface")
		println("                                       is output.")
		println("       -scale <factor>                 Allow to scale the whole model by a given factor.")
		println("       -yscale <factor>                Allow to scale only the elevation (may be used with -scale).")
		sys.exit(1)
	}
	
// Run

	def run() {
		val outFile = if(output ne null) (if(output.endsWith(".stl")) output else "%s.stl".format(output)) else null

		print("* Reading       ")
		val heightMap = HeightMapOld(input, startx, endx, starty, endy, scale, yscale)
		println(" OK")
		print("* Normalizing   ")
		heightMap.normalize()
		println("OK")
		print("* Triangulating ")
		heightMap.triangulate(volume)
		println(" OK")
		print("* Writing       [%d triangles]".format(heightMap.triangleCount))
		if(outFile eq null)
			heightMap.outputSTL("MNT", null)
		else heightMap.outputBinarySTL(outFile)
		println(" OK")
	}
}


object HeightMapOld {

	def apply(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double):HeightMapOld = {
		if(fileName.endsWith(".csv")) {
			readFileCSV(fileName, startx, endx, starty, endy, scaleFactor, yFactor)
		} else {
			throw new RuntimeException("only '.csv' file accepted")
		}
	}

	def readFileCSV(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double):HeightMapOld = {
		var heightMap:HeightMapOld = null
		val src      = new BufferedSource(new FileInputStream(fileName))
		var ncols    = 0
		var nrows    = 0
		var nodata   = 0.0
		var cellSize = 0.0
		var curRow   = 0
		var sx       = startx
		var ex       = endx
		var sy       = starty
		var ey       = endy

		import ReliefOld._

		src.getLines.foreach { _ match {
			case NCols(cols)    => { ncols = cols.toInt; if(sx < 0) sx = 0; if(ex < 0 || ex > ncols) ex = ncols }
			case NRows(rows)    => { nrows = rows.toInt; if(sy < 0) sy = 0; if(ey < 0 || ey > nrows) ey = nrows }
			case Xllcorner(yll) => { /* What is the use of this ? */ }
			case Yllcorner(yll) => { /* What is the use of this ? */ }
			case CellSize(size) => { cellSize = size.toDouble }
			case NoData(value)  => { nodata = value.toDouble }
			case line           => {
				// The format ensure informations will have been read before ?
				if(heightMap eq null) {
				 	heightMap = new HeightMapOld(ex-sx, ey-sy, nodata, cellSize, scaleFactor, yFactor)
					print("[%d x %d -> %d x %d]".format(ncols, nrows, ex-sx, ey-sy))
					heightMap.translate(sx, sy)
				}

				if(curRow % 100 == 0) print("[line %d]".format(curRow))

				if(curRow >= sy && curRow < ey) {
					val values = line.split(";").map { _.replace(",", ".").toDouble }.drop(sx)
					heightMap.setLine(curRow-sy, values)
				}	
				curRow += 1
			}
		}}

		heightMap
	}

	def readFileImage(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double):HeightMapOld = {
        val image = ImageIO.read(new File(fileName))
		var sx       = startx
		var ex       = endx
		var sy       = starty
		var ey       = endy
        
        if(sx < 0) sx = 0; if(ex < 0 || ex > image.getWidth)  ex = image.getWidth
        if(sy < 0) sy = 0; if(ey < 0 || ey > image.getHeight) ey = image.getHeight

		var heightMap = new HeightMapOld(ex-sx, ey-sy, 0, 1, scaleFactor, yFactor)
		var row = sy

		while(row < ey) {
			var col = sx
			while(col < ex) {
				heightMap.setCell(col, row, pixelToValue(image.getRGB(col, row)))
				col += 1
			}
			row += 1
		}

		heightMap
	}

	protected def pixelToValue(pixel:Int):Double = {
		val r = ((pixel >> 16) & 0xFF)
		val g = ((pixel >>  8) & 0xFF)
		val b = ((pixel      ) & 0xFF)
		val (hue, saturation, value) = Rgba(r/255.0, g/255.0, b/255.0, 1).toHSV

		hue
	}
}


/** A height map under the form of a cloud of points all aligned as a grid.
  *
  * This allows to:
  *   - read the point cloud from CSV (see companion object),
  *   - normalize it,
  *   - triangulate it,
  *   - save it to STL.
  */
class HeightMapOld(val cols:Int, val rows:Int, val nodata:Double, val cellSize:Double, val scaleFactor:Double=0.01, val yFactor:Double=1.0) {
	/** When creating a volume during triangulation, adds a base this height.
	  * This is set during triangulation. */
	protected[this] var baseDepth = 1.0

	/** The point cloud representing the height map. */
	protected[this] val data = Array.ofDim[Point3](rows, cols)

	/** The set of computed triangles for the surface from the point cloud. */
	protected[this] val triangles = new ArrayBuffer[Triangle]()

	/** The X position of this heightmap in a global file, this is used as a translation. */
	protected[this] var startx = 0.0

	/** The Y position of this heightmap in a global file, this is used as a translation. */
	protected[this] var starty = 0.0

	/** Number of generated triangles. */
	def triangleCount:Int = triangles.size

	/** Number of stored points. */
	def pointCount:Int = data.size

	/** Offset of the whole surface. */
	def offset:(Double,Double) = (startx, starty)

	/** Scale factor for the surface. */
	def scale:Double = scaleFactor

	/** If the heightmap in fact represent a part of a larger map, th offset
	  * (`startx`, `starty`) allows to place points. */
	def translate(startx:Int, starty:Int) {
		this.startx = startx
		this.starty = starty		
	}

	/** Fill a complete line of the heightmap at `row` with `values`. The
	  * values are scaled by `scaleFactor` and `yFactor`. */
	def setLine(row:Int, line:Array[Double]) {
		val n = min(cols, line.length)
		var i = 0
		while(i < n) {
			setCell(i, row, line(i))
			i += 1
		}
	}

	/** Set a cell at (`col`, `row`) in the heightmap with `value`. The `value` is
	  * scaled by `scaleFactor` and `yFactor`. */ 
	def setCell(col:Int, row:Int, value:Double) {
			data(row)(col) = Point3(/*X*/ (this.starty + row * cellSize) * scaleFactor,
								    /*Y*/ value * scaleFactor * yFactor,
								    /*Z*/ (this.startx + col * cellSize) * scaleFactor)		
	}

	/** Normalize the point cloud by aligning nodata points to the minimum point. */
	def normalize() {
		var min = -nodata
		var max = nodata
		var y = 0
		while(y < rows) {
			var x = 0
			while(x < cols) {
				val d = (data(y)(x)).y

				if(d > nodata*scaleFactor && d < min)
					min = d
				if(d > max)
					max = d

				x += 1
			}
			y += 1
		}
		print("[min %f][max %f] ".format(min, max))
		y = 0
		while(y < rows) {
			var x = 0
			while(x < cols) {
				val d = (data(y)(x)).y
				if(d <= nodata*scaleFactor)
					(data(y)(x)).y = min
				x += 1
			}
			y += 1
		}
	} 

	/** Triangulate the heightmap.
	  *
	  * This triangulate a surface from the point cloud, and adds a closed base
	  * with sides and a back so that the result is a volume.
	  * @param baseDepth the height of the base added to make a volume the total height
	  *                   is thus this base depth plus the max height of the point cloud.
	  *                   if zero or negative, only the surface is created. */
	def triangulate(baseDepth:Double = -1) {
		this.baseDepth = baseDepth
		triangulateSurface
		if(baseDepth > 0) {
			triangulateSides
			triangulateBack
		}
	}

	/** Triangulate the surface of the heightmap. */
	def triangulateSurface() {	

		// p0    p2
		//  +----+  CCW
		//  |   /|
		//  |  / |
		//  | /  |
		//  |/   |
		//  +----+
		// p1    p3

		var y = 0
		while(y < rows-1) {
			var x = 0
			while(x < cols-1) {
				val p0 = data(y)(x)
				val p1 = data(y)(x+1)
				val p2 = data(y+1)(x)
				val p3 = data(y+1)(x+1)

				triangles += ConstTriangle(p0, p2, p1)
				triangles += ConstTriangle(p1, p2, p3)
				x += 1
			}
			if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
			y += 1
		}
	}

	/** Triangulate the sides of the base. */
	def triangulateSides() {
		val base = -baseDepth*scaleFactor
		var x = 0

		// Front and back.

		while(x < cols-1) {
			var p0 = data(0)(x)
			var p1 = Point3(p0.x, base, p0.z)
			var p2 = data(0)(x+1)
			var p3 = Point3(p2.x, base, p2.z)

			triangles += ConstTriangle(p0, p2, p1)
			triangles += ConstTriangle(p1, p2, p3)

			p0 = data(rows-1)(x)
			p2 = Point3(p0.x, base, p0.z)
			p1 = data(rows-1)(x+1)
			p3 = Point3(p1.x, base, p1.z)

			triangles += ConstTriangle(p0, p2, p1)
			triangles += ConstTriangle(p1, p2, p3)

			x += 1

			if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}

		// Left and Right.

		var y = 0
		while(y < rows-1) {
			var p0 = data(y)(0)
			var p2 = Point3(p0.x, base, p0.z)
			var p1 = data(y+1)(0)
			var p3 = Point3(p1.x, base, p1.z)

			triangles += ConstTriangle(p0, p2, p1)
			triangles += ConstTriangle(p1, p2, p3)

			p0 = data(y)(cols-1)
			p1 = Point3(p0.x, base, p0.z)
			p2 = data(y+1)(cols-1)
			p3 = Point3(p2.x, base, p2.z)

			triangles += ConstTriangle(p0, p2, p1)
			triangles += ConstTriangle(p1, p2, p3)

		 	y += 1

			if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}
	}

	/** Triangulate the back of the base. */
	def triangulateBack() {
		val base = -baseDepth*scaleFactor
		val center = Point3((starty + (rows/2))*scaleFactor, -baseDepth*scaleFactor, (startx + (cols/2))*scaleFactor)
		var x = 0

		// Center toward front and back.

		while(x < cols-1) {
			var p0 = data(0)(x)
			var p1 = data(0)(x+1)
			
			triangles += ConstTriangle(Point3(p0.x, base, p0.z), Point3(p1.x, base, p1.z), center)
			
			p0 = data(rows-1)(x)
			p1 = data(rows-1)(x+1)
			
			triangles += ConstTriangle(Point3(p1.x, base, p1.z), Point3(p0.x, base, p0.z), center)
			
			x += 1

			if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}

		// Center toward left and right.

		var y = 0
		
		while(y < rows-1) {
			var p0 = data(y)(0)
			var p1 = data(y+1)(0)
			
			triangles += ConstTriangle(Point3(p1.x, base, p1.z), Point3(p0.x, base, p0.z), center)
			
			p0 = data(y)(cols-1)
			p1 = data(y+1)(cols-1)
			
			triangles += ConstTriangle(Point3(p0.x, base, p0.z), Point3(p1.x, base, p1.z), center)
			
			y += 1

			if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}
	}

	/** Output the triangulation of the point cloud as an ASCII STL file. */
	def outputSTL(name:String, fileName:String) {
		val out = if(fileName eq null) System.out else new PrintStream(new FileOutputStream(fileName))
		var i = 0
		
		out.printf("solid %s%n".format(name))
		
		triangles.foreach { triangle =>
			if(i%1000==0)print("[%d]".format(i))
			val normal = triangle.normal
			out.printf("facet normal %f %f %f%n".format(normal.x, normal.y, normal.z).replace(",","."))
			out.printf("    outer loop%n")
			out.printf("        vertex %f %f %f%n".format(triangle.p0.x, triangle.p0.y, triangle.p0.z).replace(",","."))
			out.printf("        vertex %f %f %f%n".format(triangle.p1.x, triangle.p1.y, triangle.p1.z).replace(",","."))
			out.printf("        vertex %f %f %f%n".format(triangle.p2.x, triangle.p2.y, triangle.p2.z).replace(",","."))
			out.printf("    endloop%n")
			out.printf("endfacet%n")
			i += 1
		}
		
		println("")
		out.printf("endsolid %s%n".format(name))
		out.flush
		out.close
	}

	/** Output the triangulation of the point cloud as a binary STL file.
	  * This is faster than ASCII STL and will produce quite smaller files. */
	def outputBinarySTL(fileName:String) {
		val headerEmpty = ByteBuffer(80)
		val headerB     = ByteBuffer(4)
		val header      = IntBuffer(headerB)
		val triangleB   = ByteBuffer(12*4)
		val triangle    = FloatBuffer(triangleB)
		val mysteryAttributeByteCount = ByteBuffer(2)
		
		val file    = new File(fileName)
		val channel = new FileOutputStream(file, false/*!append=overwrite*/).getChannel

		var i = 0
		while(i < 80) {
			headerEmpty(i) = 0
			i += 1
		}

		header(0) = triangles.size
		mysteryAttributeByteCount(0) = 0
		mysteryAttributeByteCount(1) = 0

		channel.write(headerEmpty.buffer.asInstanceOf[java.nio.ByteBuffer])
		channel.write(headerB.buffer.asInstanceOf[java.nio.ByteBuffer])

		i = 0
		triangles.foreach { t =>
			if(i%10000==0) print("[%d]".format(i))

			val normal = t.normal

			triangle(0)  = normal.x.toFloat
			triangle(1)  = normal.z.toFloat
			triangle(2)  = normal.y.toFloat
			triangle(3)  = t.p0.x.toFloat
			triangle(4)  = t.p0.z.toFloat
			triangle(5)  = t.p0.y.toFloat
			triangle(6)  = t.p1.x.toFloat
			triangle(7)  = t.p1.z.toFloat
			triangle(8)  = t.p1.y.toFloat
			triangle(9)  = t.p2.x.toFloat
			triangle(10) = t.p2.z.toFloat
			triangle(11) = t.p2.y.toFloat

			channel.write(triangleB.buffer.asInstanceOf[java.nio.ByteBuffer])
			channel.write(mysteryAttributeByteCount.buffer.asInstanceOf[java.nio.ByteBuffer])
			i += 1
		}
		
		channel.close
	}
}