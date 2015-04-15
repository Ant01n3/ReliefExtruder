package org.litis.relief

import scala.math._
import scala.io.BufferedSource

import org.sofa.math.{Point3, Triangle, ConstTriangle, Rgba}

import java.io.{File, InputStream, FileInputStream, FileOutputStream, PrintStream, IOException}
import javax.imageio.ImageIO


/** HeightMap companion object allowing to create height maps from CSV and PNG files. */
object HeightMap {
// Parsing

	final val NCols     = """ncols;([0-9]+);+""".r
	final val NRows     = """nrows;([0-9]+);+""".r
	final val Yllcorner = """yllcorner;([0-9]+,?[0-9]*);+""".r
	final val Xllcorner = """xllcorner;([0-9]+,?[0-9]*);+""".r
	final val CellSize  = """cellsize;([0-9]+);+""".r
	final val NoData    = """NODATA_value;(-?[0-9]+);+""".r

// HeightMap Creation from files

	def apply(fileName:String, startx:Int, endx:Int, starty:Int, endy:Int, scaleFactor:Double, yFactor:Double, iMin:Double, iMax:Double, cellSize:Double, greyData:Boolean):HeightMap = {
		if(fileName.endsWith(".csv")) {
			readFileCSV(fileName, startx, endx, starty, endy, scaleFactor, yFactor, cellSize)
		} else if(fileName.endsWith(".png")) {
			readFileImage(fileName, startx, endx, starty, endy, scaleFactor, yFactor, iMin, iMax, cellSize, greyData)
		} else {
			throw new RuntimeException("only '.csv' file accepted")
		}
	}

	/** Created a [[HeightMap]] from a CSV file. */
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

		src.getLines.foreach { _ match {
			case NCols(cols)    => { ncols = cols.toInt; if(sx < 0) sx = 0; if(ex < 0 || ex > ncols) ex = ncols }
			case NRows(rows)    => { nrows = rows.toInt; if(sy < 0) sy = 0; if(ey < 0 || ey > nrows) ey = nrows }
			case Xllcorner(yll) => { /* What is the use of this ? */ }
			case Yllcorner(yll) => { /* What is the use of this ? */ }
			case CellSize(size) => { if(cellSize == 1.0) cellsize = size.toDouble }
			case NoData(value)  => { nodata = value.toDouble }
			case line           => {
				// The format ensure informations will have been read before ?
				if(heightMap eq null) {
				 	heightMap = new HeightMap(ex-sx, ey-sy, nodata, cellsize, scaleFactor, yFactor)
					print("[%d x %d -> %d x %d]".format(ncols, nrows, ex-sx, ey-sy))
					heightMap.translate(sx, sy)
				}

				if(curRow % 100 == 0) print("[row %d]".format(curRow))

				if(curRow >= sy && curRow < ey) {
					val values = line.split(";").map { _.replace(",", ".").toDouble }.drop(sx)
					heightMap.setLine(curRow-sy, values)
				}	
				curRow += 1
			}
		}}

		heightMap
	}

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
class HeightMap(val cols:Int, val rows:Int, val nodata:Double, val cellSize:Double, val scaleFactor:Double=0.01, val yFactor:Double=1.0) {
	
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
		print("[min %f][max %f] ".format(minValue, maxValue))
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
}
