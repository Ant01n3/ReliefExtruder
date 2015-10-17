package org.litis.relief

import scala.math._

import org.sofa.math.{Point3, Rgba}
import org.sofa.nio._

import java.io.{File, IOException}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/** Base for output system of a set of points. */
trait HeightMapConverter {
	/** Begin the output, create the file or output system. */
	def begin(w:Int, h:Int, minHeight:Double, maxHeight:Double) 

	/** Append a point `p` to the file for column `x` and row `y` of the height map. */
	def point(x:Int, y:Int, p:Point3)

	/** End the output, close the file or output system. */
	def end()
}


/** A [[HeightMapConverter]] that produce a colored PNG
    to a `fileName`. The `.png` extension is added if needed. */
class ColorPNGConverter(fileName:String) extends HeightMapConverter {
	
	private var img:BufferedImage = _

	private var minHeight = 0.0

	private var maxHeight = 0.0

	def begin(w:Int, h:Int, minHeight:Double, maxHeight:Double) {
		img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
		this.minHeight = minHeight
		this.maxHeight = maxHeight
	}

	def point(x:Int, y:Int, p:Point3) {
		val h = if(maxHeight == minHeight) 0 else (p.y - minHeight) / ((maxHeight - minHeight) * 1.001)
		val clr = Rgba.fromHSV(h*2*Pi, 1, 1)
		img.setRGB(x, y, clr.toIntARGB)
	}

	def end() {
		val outputfile = new File(fileName)
		ImageIO.write(img, "png", outputfile)
	}
}
