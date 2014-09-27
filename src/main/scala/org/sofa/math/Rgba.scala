package org.sofa.math

import scala.util.Random
import scala.math._


object Rgba {
    final val Black   = new Rgba(0, 0, 0, 1)
    final val White   = new Rgba(1, 1, 1, 1)
    final val Red     = new Rgba(1, 0, 0, 1)
    final val Green   = new Rgba(0, 1, 0, 1)
    final val Blue    = new Rgba(0, 0, 1, 1)
    final val Cyan    = new Rgba(0, 1, 1, 1)
    final val Magenta = new Rgba(1, 0, 1, 1)
    final val Yellow  = new Rgba(1, 1, 0, 1)
    final val Orange  = new Rgba(1, 0.5, 0, 1)
    final val Grey10  = new Rgba(0.1, 0.1, 0.1, 1)
    final val Grey20  = new Rgba(0.2, 0.2, 0.2, 1)
    final val Grey30  = new Rgba(0.3, 0.3, 0.3, 1)
    final val Grey40  = new Rgba(0.4, 0.4, 0.4, 1)
    final val Grey50  = new Rgba(0.5, 0.5, 0.5, 1)
    final val Grey60  = new Rgba(0.6, 0.6, 0.6, 1)
    final val Grey70  = new Rgba(0.7, 0.7, 0.7, 1)
    final val Grey80  = new Rgba(0.8, 0.8, 0.8, 1)
    final val Grey90  = new Rgba(0.9, 0.9, 0.9, 1)

    def apply(from:Rgba):Rgba = new Rgba(from.red, from.green, from.blue, from.alpha)

    def apply(from:NumberSeq4):Rgba = new Rgba(from.x, from.y, from.z, from.w)

    def apply(from:NumberSeq3):Rgba = new Rgba(from.x, from.y, from.z, 1)

    def apply(from:(Double,Double,Double,Double)) = new Rgba(from._1, from._2, from._3, from._4)

    def apply(from:(Double,Double,Double)) = new Rgba(from._1, from._2, from._3, 1)

    def apply(from:java.awt.Color):Rgba = new Rgba(from.getRed/255.0, from.getGreen/255.0, from.getBlue/255.0, from.getAlpha/255.0)

    def apply(r:Int, g:Int, b:Int):Rgba = apply(r,g,b,255)

    def apply(r:Int, g:Int, b:Int, a:Int):Rgba = new Rgba(r/255.0, g/255.0, b/255.0, a/255.0)
    
    def apply(r:Double, g:Double, b:Double, a:Double):Rgba = new Rgba(r, g, b, a)
    
    def apply(r:Double, g:Double, b:Double):Rgba = new Rgba(r, g, b, 1)

    def unapply(from:Rgba):Some[(Double,Double,Double,Double)] = Some(from.rgba)

    /** A random RGB color with alpha to 1. Values for red, green and blue are taken in
      * randomly in [0:1]. */
    def random():Rgba = Rgba(Random.nextDouble, Random.nextDouble, Random.nextDouble, 1.0)

    /** A color with random hue, but fixed `saturation` and `value`. */
    def randomHue(saturation:Double, value:Double):Rgba = fromHSV((Random.nextDouble*(2*Pi)), saturation, value)

    /** A color with random hue, and saturation but fixed `value`. */
    def randomHueAndSaturation(value:Double):Rgba = fromHSV((Random.nextDouble*(2*Pi)), Random.nextDouble, value)

    /** A color with random hue, and value but fixed `saturation`. */
    def randomHueAndValue(saturation:Double):Rgba = fromHSV((Random.nextDouble*(2*Pi)), saturation, Random.nextDouble)

    /** A color with random saturation, and value but fixed `hue`. */
    def randomSaturationAndValue(hue:Double):Rgba = fromHSV(hue, Random.nextDouble, Random.nextDouble)

    /** A color with random saturation, but fixed `hue` and `value`. */
    def randomSaturation(hue:Double, value:Double):Rgba = fromHSV(hue, Random.nextDouble, value)

    /** A color with random value, but fixed `hue` and `saturation`. */
    def randomValue(hue:Double, saturation:Double):Rgba = fromHSV(hue, saturation, Random.nextDouble)

    /** Create a RGBA color from the HSV (`hue`, `saturation`, `value`) specification. The alpha is 1.
      * The `hue` is given in radians, whereas the `saturation` and `value` are given in [0:1]. */
    def fromHSV(hue:Double, saturation:Double, value:Double):Rgba = fromHSV(hue, saturation, value, 1.0)

    /** Create a RGBA color from the HSV (`hue`, `saturation`, `value`) and `alpha` specification.
      * The `hue` is given in radians, whereas the `saturation` and `value` are given in [0:1]. */
    def fromHSV(hue:Double, saturation:Double, value:Double, alpha:Double) = {
    	val C = value * saturation		// chroma
    	val H = (hue / (Pi/3.0))		// H' = hue / 60°
    	val X = C * (1.0 - abs(H % 2.0 - 1.0))

    	var R1 = 0.0
    	var G1 = 0.0
    	var B1 = 0.0

    	if(H >=0 && H < 1) {
    		R1 = C
    		G1 = X
    		B1 = 0
    	} else if(H >= 1 && H < 2) {
    		R1 = X
    		G1 = C
    		B1 = 0
    	} else if(H >= 2 && H < 3) {
    		R1 = 0
    		G1 = C
    		B1 = X
    	} else if(H >= 3 && H < 4) {
    		R1 = 0
    		G1 = X
    		B1 = C
    	} else if(H >= 4 && H < 5) {
    		R1 = X
    		G1 = 0
    		B1 = C
    	} else if(H >= 5 && H < 6) {
    		R1 = C
    		G1 = 0
    		B1 = X
    	} else {
    		throw new RuntimeException("unexpected H value %f not in [0:6[".format(H))
    	}

    	val m = value - C
//println("HSV(%f, %f, %f) -> RGB(%f, %f, %f)".format(hue, saturation, value, R1+m, G1+m, B1+m))
    	Rgba(R1+m, G1+m, B1+m, alpha)
    }
}

/* This should maybe inherit NumberSeq4 ??! */

/** A simple color description made of four double numbers.
  * Meaningfull values are between 0 and 1, but no automatic clamping is done,
  * and larger values are allowed. */
class Rgba(
	var red:Double, 
	var green:Double,
	var blue:Double,
	var alpha:Double) {

	/** Make a copy of `other` into this. */
	def copy(other:Rgba) { 
		red   = other.red
		green = other.green
		blue  = other.blue
		alpha = other.alpha
	}
	
	/** New instance containing the multiplication of each component by `factor`. */
	def * (factor:Double):Rgba = {
		new Rgba(red*factor, green*factor, blue*factor, alpha*factor)
	}

	/** Multiply in place each component by `factor`. */
	def *= (factor:Double):Rgba = {
		red   *= factor
		green *= factor
		blue  *= factor
		alpha *= factor
		this
	}

	/** New instance containing the addition component-wise of this and `other`. */
	def + (other:Rgba):Rgba = {
		new Rgba(red + other.red, green + other.green, blue + other.blue, alpha + other.alpha)
	}

	/** Add component-wise each component of `other` to this. */
	def += (other:Rgba):Rgba = {
		red   += other.red
		green += other.green
		blue  += other.blue
		alpha += other.alpha
		this
	}

	/** Multiply in place red, green and blue components by alpha. */
	def alphaPremultiply() {
		red   *= alpha
		green *= alpha
		blue  *= alpha
	}

	/** Convert the rgb part to a normal vector as used in normal maps.
	  * The red, green, blue values are mapped from [0:1] to [-1:1]. It must not be alpha premultiplied. */
	def toNormal() = Vector3(
		// (red   * 2.0) - 1.0,
		// (green * 2.0) - 1.0,
		// (blue  * 2.0) - 1.0
		(red   - 0.5) * 2.0,
		(green - 0.5) * 2.0,
		(blue  - 0.5) * 2.0
	)

	/** New copy of this with red, green and blue components multiplied by alpha. Alpha is unchanged. */
	def alphaPremultiplied():Rgba = Rgba(red*alpha, green*alpha, blue*alpha, alpha)

	/** A 3-tuple for the red, green and blue components. */
	def rgb:(Double,Double,Double) = (red,green,blue)

	/** Change the red, green and blue components using a 3-tuple. */
	def rgb_=(rgb:(Double,Double,Double)) { red = rgb._1; green = rgb._2; blue = rgb._3 }

	/** A 4-tuple for the red, green, blue and alpha components. */
	def rgba:(Double,Double,Double,Double) = (red,green,blue,alpha)

	/** Change the red, green, blue, alpha components using a 4-tuple. */
	def rgba_=:(rgba:(Double,Double,Double,Double)) {  red = rgba._1; green = rgba._2; blue = rgba._3; alpha = rgba._4 }

	/** Ensure each component is between 0 and 1 inclusive. */
	def clamp() {
		if(red   > 1) red   = 1.0 else if(red   < 0) red   = 0.0
		if(green > 1) green = 1.0 else if(green < 0) green = 0.0
		if(blue  > 1) blue  = 1.0 else if(blue  < 0) blue  = 0.0
		if(alpha > 1) alpha = 1.0 else if(alpha < 0) alpha = 0.0
	}

	/** Convert this color to its HSV representation. The hue is in radians, the
	  * saturation and value are between 0 and 1. The returned triplet is in this
	  * order (hue, staturation, value). */
	def toHSV:(Double,Double,Double) = {
		val cmax = max(red, max(green, blue))
		val cmin = min(red, min(green, blue))
		val delta = cmax - cmin
		if(delta != 0) {
			val saturation = if(delta == 0) 0 else delta / cmax
			val value = cmax
			val hue = if(cmax == red) {
				(Pi/3.0) * (((green - blue) / delta) % 6) 
			} else if(cmax == green) {
				(Pi/3.0) * ((blue - red) / delta + 2)
			} else {
				(Pi/3.0) * ((red - green) / delta + 4)
			}

			(hue, saturation, value)
		} else {
			(0.0, 0.0, red) // white, grey or black, all components the same value.
		}
	}

	override def toString():String = "RGBA[%.3f %.3f %.3f %.3f]".format(red, green, blue, alpha)
}