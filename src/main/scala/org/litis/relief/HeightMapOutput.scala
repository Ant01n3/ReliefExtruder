package org.litis.relief

import org.sofa.math.{Point3, Triangle, ConstTriangle, Rgba}
import org.sofa.nio._

import java.io.{File, InputStream, FileInputStream, FileOutputStream, PrintStream, IOException}
import java.nio.channels.FileChannel


/** Base for output system of a set of triangles. */
trait HeightMapOutput {
	/** Begin the output, create the file or output system. */
	def begin() 

	/** Append a triangle to the file. */
	def triangle(t:Triangle)

	/** End the output, close the file or output system. */
	def end()
}


/** A HeightMapOutput that produce an ASCII STL format, either to a file or 
  * to the standard output if `fileName` is null. */
class AsciiSTLOutput(name:String, fileName:String) extends HeightMapOutput {
	
	private[this] var out:PrintStream = null

	def begin() {
		if(out eq null) {
			out = if(fileName eq null) System.out else new PrintStream(new FileOutputStream(fileName))
			out.printf("solid %s%n".format(name))
		}
	}

	def triangle(triangle:Triangle) {
		if(out ne null) {
			val normal = triangle.normal
			out.printf("facet normal %f %f %f%n".format(normal.x, normal.y, normal.z).replace(",","."))
			out.printf("    outer loop%n")
			out.printf("        vertex %f %f %f%n".format(triangle.p0.x, triangle.p0.y, triangle.p0.z).replace(",","."))
			out.printf("        vertex %f %f %f%n".format(triangle.p1.x, triangle.p1.y, triangle.p1.z).replace(",","."))
			out.printf("        vertex %f %f %f%n".format(triangle.p2.x, triangle.p2.y, triangle.p2.z).replace(",","."))
			out.printf("    endloop%n")
			out.printf("endfacet%n")
		}
	}

	def end() {
		if(out ne null) {
			println("")
			out.printf("endsolid %s%n".format(name))
			out.flush
			out.close
			out = null
		}
	}
}


/** A HeightMapOutput that produce a binary STL file. */
class BinarySTLOutput(val name:String, val fileName:String, val triangleCount:Int) extends HeightMapOutput {
	
	private[this] var channel:FileChannel = null
	
	private[this] val triangleB = ByteBuffer(12*4)
	
	private[this] val triangle = FloatBuffer(triangleB)
	
	private[this] val mysteryAttributeByteCount = ByteBuffer(2)

	def begin() {
		val headerEmpty = ByteBuffer(80)
		val headerB     = ByteBuffer(4)
		val header      = IntBuffer(headerB)
		
		channel = new FileOutputStream(new File(fileName), false/*!append=overwrite*/).getChannel

		var i = 0
		while(i < 80) { headerEmpty(i) = 0; i += 1 }

		header(0) = triangleCount
		mysteryAttributeByteCount(0) = 0
		mysteryAttributeByteCount(1) = 0

		channel.write(headerEmpty.buffer.asInstanceOf[java.nio.ByteBuffer])
		channel.write(headerB.buffer.asInstanceOf[java.nio.ByteBuffer])
	}

	def triangle(t:Triangle) {
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
	}

	def end() {
		channel.close
	}
}