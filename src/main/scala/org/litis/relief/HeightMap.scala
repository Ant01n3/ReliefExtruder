package org.litis.relief

import scala.math._
import scala.io.BufferedSource

import org.sofa.math.{Point3, Triangle, ConstTriangle, Rgba}

import java.io.{File, InputStream, FileInputStream, FileOutputStream, PrintStream, IOException}
import javax.imageio.ImageIO


/** Techniques of resizing of the heightmap. */
object ResizeMethod extends Enumeration {
	val Lanczos2 = Value
	val Lanczos3 = Value
	val Lanczos4 = Value
	val Hq3x = Value
	val Hq4x = Value
	val Unknown = Value

	type ResizeMethod = Value

	def fromString(s:String):ResizeMethod = s.toLowerCase match {
		case "lanczos2" => Lanczos2
		case "lanczos3" => Lanczos3
		case "lanczos4" => Lanczos4
		case "hq3x" => Hq3x 
		case "hq4x" => Hq4x
		case _ => Unknown
	}
}


/** HeightMap companion object allowing to create height maps from CSV and PNG files. */
object HeightMap {
// Parsing

	final val NCols    = """ncols;([0-9]+);+""".r
	final val NColsSp  = """ncols\s+([0-9]+)\s*""".r
	final val NRows    = """nrows(;|\s+)([0-9]+)(;+|\s*)""".r
	final val Yll      = """yll(corner|center)(;|\s+)([0-9]+[\.,]?[0-9]*)(;+|\s*)""".r
	final val Xll      = """xll(corner|center)(;|\s+)([0-9]+[\.,]?[0-9]*)(;+|\s*)""".r
	final val CellSize = """cellsize(;|\s+)([0-9]+[\.,]?[0-9]*)(;+|\s*)""".r
	final val NoData   = """(NODATA|nodata)_value(;|\s+)(-?[0-9]+)(;+|\s*)""".r

// HeightMap Creation from files

	def apply(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double, iMin:Double, iMax:Double, cellSize:Double, greyData:Boolean):HeightMap = {
		if(fileName.endsWith(".csv") || fileName.endsWith(".asc")) {
			readFileCSV(fileName, startx, endx, starty, endy, scaleFactor, yFactor, cellSize)
		} else if(fileName.endsWith(".png")) {
			readFileImage(fileName, startx, endx, starty, endy, scaleFactor, yFactor, iMin, iMax, cellSize, greyData)
		} else {
			throw new RuntimeException("only '.csv', '.asc' and '.png' files are accepted")
		}
	}

	/** Created a [[HeightMap]] from a CSV or ASC file. */
	def readFileCSV(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double, cellSize:Double):HeightMap = {
		var heightMap:HeightMap = null
		val src      = new BufferedSource(new FileInputStream(fileName))
		var ncols    = 0
		var nrows    = 0
		var nodata   = 0.0
		var cellsize = cellSize
		var curRow   = 0
		var sx       = startx
		var ex       = endx
		var sy       = starty
		var ey       = endy
		var spaceSeparated = false
		var latm     = 0.0
		var lonm     = 0.0

		src.getLines.foreach { _ match {
			case NCols(cols)         => { ncols = cols.toInt; if(sx < 0) sx = 0; if(ex < 0 || ex > ncols) ex = ncols; spaceSeparated = false }
			case NColsSp(cols)       => { ncols = cols.toInt; if(sx < 0) sx = 0; if(ex < 0 || ex > ncols) ex = ncols; spaceSeparated = true }
			case NRows(_,rows,_)     => { nrows = rows.toInt; if(sy < 0) sy = 0; if(ey < 0 || ey > nrows) ey = nrows }
			case Xll(c,_,xll,_)      => { latm = xll.toDouble /* easting coordinate. */ }
			case Yll(c,_,yll,_)      => { lonm = yll.toDouble /* northing coordinate. */ }
			case CellSize(_,size,_)  => { if(cellSize == 1.0) cellsize = size.toDouble }
			case NoData(a,b,value,c) => { nodata = value.toDouble }
			case line                => {
				// The format ensure informations will have been read before ?
				if(heightMap eq null) {
					latm = 49.4899
					lonm = 0.1099
					printf("[(%f %f) -> %f lat %f lon -> %f lat %f lon]%n", latm, lonm, y2lat_m(latm), x2lon_m(lonm), lat2y_m(latm), lon2x_m(lonm))

				 	heightMap = new HeightMap(ex-sx, ey-sy, nodata, cellsize, scaleFactor, yFactor)
					print("[%d x %d -> %d x %d (spaces=%b)]".format(ncols, nrows, ex-sx, ey-sy, spaceSeparated))
					heightMap.translate(sx, sy)
//printf("sx=%d ex=%d sy=%d ey=%d ncols=%d nrows=%d size=%f nodata=%f%n", sx, ex, sy, ey, ncols, nrows, cellSize, nodata)
				}

				if(curRow % 100 == 0) print("[row %d]".format(curRow))

				if(curRow >= sy && curRow < ey) {
					val values = if(spaceSeparated)
					     line.trim.split("\\s+").slice(sx,ex).map { _.replace(",", ".").toDouble }
					else line.trim.split(";").slice(sx,ex).map { _.replace(",", ".").toDouble }
					heightMap.setLine(curRow-sy, values)
				}
				curRow += 1
			}
		}}

		heightMap
	}


	def deg2rad(d:Double) = (((d)*Pi)/180.0)
	def rad2deg(d:Double) = (((d)*180.0)/Pi)
	val earth_radius = 6378137
 
// /* The following functions take or return there results in degrees */
 
// double y2lat_d(double y) { return rad2deg(2 * atan(exp(  deg2rad(y) ) ) - M_PI/2); }
// double x2lon_d(double x) { return x; }
// double lat2y_d(double lat) { return rad2deg(log(tan(M_PI/4+ deg2rad(lat)/2))); }
// double lon2x_d(double lon) { return lon; }
 
/* The following functions take or return there results in something close to meters, along the equator */
 
	def y2lat_m(y:Double) = rad2deg(2 * atan(exp( (y / earth_radius ) )) - Pi/2)
	def x2lon_m(x:Double) = rad2deg(x / earth_radius)
	def lat2y_m(lat:Double) = earth_radius * log(tan(Pi/4+ deg2rad(lat)/2))
	def lon2x_m(lon:Double) = deg2rad(lon) * earth_radius

	//def y2lat(aY:Double) = Math.toDegrees(2* Math.atan(Math.exp(Math.toRadians(aY))) - Math.PI/2)
	//def lat2y(aLat:Double) = Math.toDegrees(Math.log(Math.tan(Math.PI/4+Math.toRadians(aLat)/2)))

	/** Create a [[HeigtMap]] from a PNG image. */
	def readFileImage(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double, iMin:Double, iMax:Double, cellSize:Double, greyData:Boolean):HeightMap = {
        val image = ImageIO.read(new File(fileName))
		var sx    = startx
		var ex    = endx
		var sy    = starty
		var ey    = endy
        
		print("[%d x %d -> %d x %d]".format(image.getWidth, image.getHeight, ex-sx, ey-sy))

        if(sx < 0) sx = 0; if(ex < 0 || ex > image.getWidth)  ex = image.getWidth
        if(sy < 0) sy = 0; if(ey < 0 || ey > image.getHeight) ey = image.getHeight

		var heightMap = new HeightMap(ex-sx, ey-sy, 0, cellSize, scaleFactor, yFactor)
		var row = sy

		while(row < ey) {
			var col = sx
			while(col < ex) {
				if(greyData)
				     heightMap.setCell(col, row, pixelToValueFromGrey(image.getRGB(col, row), iMin, iMax))
				else heightMap.setCell(col, row, pixelToValueFromHue(image.getRGB(col, row), iMin, iMax))
				col += 1
			}
			if(row % 100 == 0) print("[row %d]".format(row))
			row += 1
		}

		heightMap
	}

	/** Convert a `rgb` considered as grey into an elevation using the red component only.
	  * The resulting value is scaled between `iMin` and `iMax`. */
	protected def pixelToValueFromGrey(rgb:Int, iMin:Double, iMax:Double):Double = {
		val g = ((rgb >> 16) & 0xFF)	// The red component...
		val c = g / 255.0

		iMin + (c * (iMax - iMin))
	}

	/** Convert a `rgb` pixel into an elevation using the hue only (not the saturation, nor the value).
	  * The resulting hue is scaled between `iMin` and `iMax`. */
	protected def pixelToValueFromHue(rgb:Int, iMin:Double, iMax:Double):Double = {
		val r = ((rgb >> 16) & 0xFF)
		val g = ((rgb >>  8) & 0xFF)
		val b = ((rgb      ) & 0xFF)
		val (hue, saturation, value) = Rgba(r/255.0, g/255.0, b/255.0, 1).toHSV
		var res = (1.0-(hue/(2*Pi)))

		if(res == 1.0) res = 0.0 	// Special case of white pixels = nodata

		res = iMin + (res * (iMax - iMin))

		res
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
class HeightMap(val cols:Int, val rows:Int, val nodata:Double, val cellSize:Double, val scaleFactor:Double=1.0, val yFactor:Double=1.0) {
	
	import ResizeMethod._

	/** When creating a volume during triangulation, adds a base this height.
	  * This is set during triangulation. */
	protected var baseDepth = 1.0

	/** The point cloud representing the height map. */
	protected val data = Array.ofDim[Point3](rows, cols)

	/** The output for the heightmap during triangulation. */
	protected var output:HeightMapOutput = null 

	/** The X position of this heightmap in a global file, this is used as a translation. */
	protected var startx = 0.0

	/** The Y position of this heightmap in a global file, this is used as a translation. */
	protected var starty = 0.0

	/** Computed minimum value in the heightmap. */
	protected var minValue = Double.MaxValue

	/** Computed maximum value in the heightmap. */
	protected var maxValue = Double.MinValue

	/** Number of stored points. */
	def pointCount:Int = data.size

	/** Number of triangles that will be generated. Use `setVolume()` before calling
	  * this if you wan the base triangles to be counted. */
	def triangleCount:Int = surfaceTriangleCount + (if(baseDepth <= 0) 0 else baseTriangleCount)

	/** Number of triangles per surface. */
	def surfaceTriangleCount():Int = (rows-1) * (cols-1) * 2

	/** Number of triangles on the base. */
	def baseTriangleCount():Int = ((cols-1) * 6) + ((rows-1) * 6)

	/** Offset of the whole surface. This allows to translate the whole generated model,
	  * if the model is a part of a larger one, so that the sub-model is at the correct
	  * coordinates inside the larger one. */
	def offset:(Double,Double) = (startx, starty)

	/** Scale factor for the whole generated model. */
	def scale:Double = scaleFactor

	/** If the heightmap represents a part of a larger map, the offset
	  * (`startx`, `starty`) allows to place the generated model inside the larger one. */
	def translate(startx:Int, starty:Int) {
		this.startx = startx
		this.starty = starty		
	}

	/** Add a base to the surface. If set to <= 0, do not produce a base. The base
	  * creates a volume instead of only a surface when triangulating. The heigh of the
	  * base is added to the full height of the surface.
	  * @param baseDepth the height of the base added to make a volume the total height
	  *                   is thus this base depth plus the max height of the point cloud.
	  *                   if zero or negative, only the surface is created. */
	def setVolume(baseDepth:Double) {
		this.baseDepth = baseDepth
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

	/** Value of the cell at (`col`, `row`). */
	def cell(col:Int, row:Int):Point3 = data(row)(col)

	/** Height at (`col`, `row`). */
	def height(col:Int, row:Int):Double = cell(col, row).y

	/** Lowest height. */
	def minHeight:Double = minValue

	/** Highest height. */
	def maxHeight:Double = maxValue

	/** Set a cell at (`col`, `row`) in the heightmap with `value`. The `value` is
	  * scaled by `scaleFactor` and `yFactor`. */ 
	def setCell(col:Int, row:Int, value:Double) {
		val v = value * scaleFactor * yFactor

		if(value != nodata) {
			if(v < minValue) minValue = v
			if(v > maxValue) maxValue = v			
		}

		data(row)(col) = Point3(/*X*/ (this.starty + row * cellSize) * scaleFactor,
							    /*Y*/ v,
							    /*Z*/ (this.startx + col * cellSize) * scaleFactor)		
	}

	/** Normalize the point cloud by aligning nodata points to the minimum point. */
	def normalize() {
		var y = 0
		while(y < rows) {
			var x = 0
			while(x < cols) {
				val d = (data(y)(x)).y

				if(d == nodata*scaleFactor*yFactor && d < minValue) {
					(data(y)(x)).y = minValue
//					print("[%f]".format(d))
				}
				x += 1
			}
			y += 1
		}
	} 

	/** Interpolate a new height-map with a different resolution. */
	def resize(factor:Double, resizeMethod:ResizeMethod = Lanczos2):HeightMap = {
		val colsTo = round(cols * factor).toInt
		val rowsTo = round(rows * factor).toInt
		val hmap   = new HeightMap(colsTo, rowsTo, nodata, cellSize, scaleFactor, yFactor)
		var row    = 0
		var col    = 0
		val interpolator = chooseInterpolator(resizeMethod)

		while(row < rowsTo) {
			col = 0
			while(col < colsTo) {
				hmap.setCell(col, row, interpolator(col, row, colsTo, rowsTo))
				col += 1
			}
			row += 1
			if(row % 100 == 0)
				printf("[row %d]", row)
		}

		hmap
	}

	private def chooseInterpolator(resizeMethod:ResizeMethod):(Int,Int,Int,Int)=>Double = resizeMethod match {
		case Lanczos2 => interpolateLanczos(_, _, _, _, 2)
		case Lanczos3 => interpolateLanczos(_, _, _, _, 3)
		case Lanczos4 => interpolateLanczos(_, _, _, _, 4)
		case Hq3x     => throw new RuntimeException("HQ3x TODO")
		case Hq4x     => throw new RuntimeException("HQ4x TODO")
		case _        => throw new RuntimeException("Unknown resize method")
	}

	private def interpolateLanczos(xTo:Int, yTo:Int, colsTo:Int, rowsTo:Int, a:Int = 2):Double = {
		val xratio = cols.toDouble / colsTo
		val yratio = rows.toDouble / rowsTo
		val x = xTo * xratio
		val y = yTo * yratio
		val x_ = floor(x).toInt
		val y_ = floor(y).toInt
		var acc = 0.0
		var i = 0			// along X
		var j = y_ - a + 1	// along Y
		var w = 0.0

		while(j <= y_ + a) {
			i = x_ - a + 1
			while(i <= x_ + a) {
				if(i >= 0 && i < cols && j >= 0 && j < rows) {
					val l = lanczos(x - i, a) * lanczos(y - j, a)
					acc += ((height(i, j) / scaleFactor) / yFactor) * l
					w += l
				}
				i += 1
			}
			j += 1
		}

		if(w != 0)
			acc / w 	// The lanczos coefficients do not always add to 1.
		else acc
	}


	private def lanczos(x:Double, a:Double):Double = {
	//private def lanczos(x:Double, a:Double):Double = if(x != 0) (a * sin(Pi * x) * sin(Pi * x / a) / (Pi*Pi * x*x)) else 1
		if(x == 0) 1.0
		else if(x <= a && x >= a) 0.0
		else {
			val pi_x = x * Pi
			a * sin(pi_x) * sin(pi_x / a) / (pi_x * pi_x)
		} 
	}

	/** Triangulate the heightmap.
	  *
	  * This triangulate a surface from the point cloud, and adds a closed base
	  * with sides and a back so that the result is a volume if `setVolume()` as
	  * been set. */
	def triangulate() {
		triangulateSurface
		if(baseDepth > 0) {
			triangulateSides
			triangulateBack
		}
	}

	/** Triangulate the surface of the heightmap. */
	def triangulateSurface() {	

		// p0    p2
		//  +----+  CW
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

				triangle(ConstTriangle(p0, p2, p1))
				triangle(ConstTriangle(p1, p2, p3))
				x += 1
			}

			if((y % 100) == 0)
				print("[row %d]".format(y))
			
			y += 1
		}
	}

	/** Triangulate the sides of the base. */
	def triangulateSides() {
		val base = minValue - (baseDepth * scaleFactor * yFactor)
		var x = 0

		print("[sides]")

		// Front and back.

		while(x < cols-1) {
			var p0 = data(0)(x)
			var p1 = Point3(p0.x, base, p0.z)
			var p2 = data(0)(x+1)
			var p3 = Point3(p2.x, base, p2.z)

			triangle(ConstTriangle(p0, p2, p1))
			triangle(ConstTriangle(p1, p2, p3))

			p0 = data(rows-1)(x)
			p2 = Point3(p0.x, base, p0.z)
			p1 = data(rows-1)(x+1)
			p3 = Point3(p1.x, base, p1.z)

			triangle(ConstTriangle(p0, p2, p1))
			triangle(ConstTriangle(p1, p2, p3))

			x += 1

			//if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}

		// Left and Right.

		var y = 0
		while(y < rows-1) {
			var p0 = data(y)(0)
			var p2 = Point3(p0.x, base, p0.z)
			var p1 = data(y+1)(0)
			var p3 = Point3(p1.x, base, p1.z)

			triangle(ConstTriangle(p0, p2, p1))
			triangle(ConstTriangle(p1, p2, p3))

			p0 = data(y)(cols-1)
			p1 = Point3(p0.x, base, p0.z)
			p2 = data(y+1)(cols-1)
			p3 = Point3(p2.x, base, p2.z)

			triangle(ConstTriangle(p0, p2, p1))
			triangle(ConstTriangle(p1, p2, p3))

		 	y += 1

			//if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}
	}

	/** Triangulate the back of the base. */
	def triangulateBack() {
		val base = minValue - (baseDepth * scaleFactor * yFactor)
		val center = Point3((starty + (rows/2))*scaleFactor, base, (startx + (cols/2))*scaleFactor)
		var x = 0

		print("[back]")

		// Center toward front and back.

		while(x < cols-1) {
			var p0 = data(0)(x)
			var p1 = data(0)(x+1)
			
			triangle(ConstTriangle(Point3(p0.x, base, p0.z), Point3(p1.x, base, p1.z), center))
			
			p0 = data(rows-1)(x)
			p1 = data(rows-1)(x+1)
			
			triangle(ConstTriangle(Point3(p1.x, base, p1.z), Point3(p0.x, base, p0.z), center))
			
			x += 1

			//if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}

		// Center toward left and right.

		var y = 0
		
		while(y < rows-1) {
			var p0 = data(y)(0)
			var p1 = data(y+1)(0)
			
			triangle(ConstTriangle(Point3(p1.x, base, p1.z), Point3(p0.x, base, p0.z), center))
			
			p0 = data(y)(cols-1)
			p1 = data(y+1)(cols-1)
			
			triangle(ConstTriangle(Point3(p0.x, base, p0.z), Point3(p1.x, base, p1.z), center))
			
			y += 1

			//if(triangles.size % 100 == 0) print("[%d]".format(triangles.size))
		}
	}

	/** Start the output of triangles generated during the triangulation phase to a STL file.
	  * Follow this call by several calls to `triangle()` or call `triangulate()`. Finish the
	  * output using `endSTL()`.
	  * @param name The name of the mesh.
	  * @param fileName The output file name, if null and the output is binary, the result is sent to the standard output.
	  * @param binary If true (the default) output a more compact binary file, else an ascii file. */
	def beginSTL(name:String, fileName:String, binary:Boolean = true) {
		if(output eq null) {
			if(binary) {
				output = new BinarySTLOutput(name, fileName, triangleCount)
			} else {
				output = new AsciiSTLOutput(name, fileName)
			}
			
			output.begin
		}
	}

	/** Output a triangle to the current STL file, `beginSTL()` must have been called. */
	def triangle(t:Triangle) {
		if(output ne null) output.triangle(t)
	}

	/** End the output to the current STL file, started by `beginSTL()`. */
	def endSTL() {
		if(output ne null) {
			output.end
			output = null
		}
	}

	/** Convert the heightmap surface to a PNG image with the given `fileName`.
	  * If `grey` is true, the values are between 0 and 255. Else use a chromatic
	  * circle to code color. */
	def toPNG(fileName:String, grey:Boolean =  false) {
		val converter = if(grey) new GreyPNGConverter(fileName) else new ColorPNGConverter(fileName)

		converter.begin(cols, rows, minValue, maxValue)
		var x = 0
		var y = 0
		while(y < rows) {
			x = 0
			while(x < cols) {
				converter.point(x, y, cell(x, y))
				x += 1
			}
			y += 1
		}
		converter.end
	}
}
