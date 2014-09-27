package org.sofa.math


import scala.language.implicitConversions


//===================================================


object Vector {
    def apply(values:Double*) = {
        val result = new Vector(values.size)
        result.copy(values)
        result
    }
    def apply(other:NumberSeq) = {
        val result = new Vector(other.size)
        result.copy(other)
        result
    }
    def apply(size:Int) = new Vector(size)
}


class Vector(size:Int) extends NumberSeq {
    type ReturnType = Vector
    
    protected[math] final val data = new Array[Double](size)
    def this(other:NumberSeq) = { this(other.size); copy(other) } 
    def newInstance = new Vector(size)
}


//===================================================


object Vector2 {
    implicit def vector2ToTuple(v:Vector2):(Double, Double) = (v.x, v.y)
    def apply(x:Double, y:Double) = new Vector2(x, y)
    def apply() = new Vector2()
    def apply(from:Point2, to:Point2) = new Vector2(to.x-from.x, to.y-from.y)
    def apply(other:NumberSeq) = {
        if(other.size < 1) // Nooooo !!!
             new Vector2()
        else if(other.size < 2)
        	 new Vector2(other.data(0), 0)
        else new Vector2(other.data(0), other.data(1))
    }
    def apply(xy:(Double, Double)) = new Vector2(xy._1, xy._2)
    def apply(fill:Double) = new Vector2(fill, fill)
}


class Vector2(xInit:Double, yInit:Double) extends NumberSeq2 {
    type ReturnType = Vector2
    
    protected[math] final val data = Array[Double](xInit, yInit)
    def this(other:Vector2) = this(other.x, other.y)
    def this() = this(0, 0)
    def newInstance = new Vector2
    override final def size:Int = 2
}


//===================================================


object Vector3 {
    implicit def vector3ToTuple(v:Vector3):(Double, Double, Double) = (v.x, v.y, v.z)
    def apply(x:Double, y:Double, z:Double) = new Vector3(x, y, z)
    def apply() = new Vector3()
    def apply(from:Point3, to:Point3) = new Vector3(to.data(0)-from.data(0), to.data(1)-from.data(1), to.data(2)-from.data(2))
    def apply(other:NumberSeq) = {
        if(other.size < 1) // Nooooo !!!
             new Vector3()
        else if(other.size < 2)
             new Vector3(other.data(0), 0, 0)
        else if(other.size < 3)
             new Vector3(other.data(0), other.data(1), 0)
        else new Vector3(other.data(0), other.data(1), other.data(2))
    }
    def apply(other:Vector3) = new Vector3(other.data(0), other.data(1), other.data(2))
    def apply(xyz:(Double, Double, Double)) = new Vector3(xyz._1, xyz._2, xyz._3)
    def apply(xy:(Double, Double), z:Double) = new Vector3(xy._1, xy._2, z)
    def apply(x:Double, yz:(Double, Double)) = new Vector3(x, yz._1, yz._2)
    def apply(fill:Double) = new Vector3(fill, fill, fill)
}


class Vector3(xInit:Double, yInit:Double, zInit:Double) extends NumberSeq3 {
    /** Set this to the cross product of this and vector (`x`, `y`, `z`).
      *
      * This operation works in place, modifying this vector.
      */
	def cross(x:Double, y:Double, z:Double) {
		var xx = 0.0
		var yy = 0.0

		xx      = (data(1) * z) - (data(2) * y);
		yy      = (data(2) * x) - (data(0) * z);
		data(2) = (data(0) * y) - (data(1) * x);
		data(0) = xx
		data(1) = yy
	}
    
    /** Set this as the vector between points `from` and `to`. */
    def set(from:Point3, to:Point3) {
        val f = from.data
        val t = to.data
        data(0) = t(0)-f(0)
        data(1) = t(1)-f(1)
        data(2) = t(2)-f(2)
    }

    /** Set this to the cross product of this and `other`.
      *
      * This operation works in place, modifying this vector.
      */
	def cross(other:Vector3):ReturnType = {
		var xx = 0.0
		var yy = 0.0
        val o = other.data

		xx      = (data(1) * o(2)) - (data(2) * o(1));
		yy      = (data(2) * o(0)) - (data(0) * o(2));
		data(2) = (data(0) * o(1)) - (data(1) * o(0));
		data(0) = xx
		data(1) = yy

		this
	}
	
	/** Result of the cross product between this and an `other` vector.
	  * 
	  * @return A new vector result of the cross product. 
	  */
	def X(other:Vector3):ReturnType = newClone.cross(other).asInstanceOf[ReturnType]

    def copy(other:Vector3) {
        // Much faster than original on n elements.
        val o = other.data
        data(0) = o(0)
        data(1) = o(1)
        data(2) = o(2)
    }

    override def norm:Double = math.sqrt(data(0)*data(0) + data(1)*data(1) + data(2)*data(2))
    
    override def normalize():Double = {
        // Much faster than original on n elements.
        val len = norm
        data(0) /= len
        data(1) /= len
        data(2) /= len
        len
    }

    /** Switch axes from the blender convention. The x of remains the one of blender.
      * However the y becomes z of blender and the z become the -y of blender. In
      * other terms:
      *
      *     x =  xb
      *     y =  zb
      *     z = -yb
      *
      * When x, y, z are our coordinates and xb, yb, zb the ones of blender. */
    def fromBlender() {
    	val z = -data(1)
    	data(1) = data(2)
    	data(2) = z
    }

    def +(other:Vector3):ReturnType = (new Vector3(data(0), data(1), data(2))).addBy(other)   // Faster than using apply
    
    override def +(value:Double):ReturnType = (new Vector3(data(0), data(1), data(2))).addBy(value)   // Faster than using apply

    def -(other:Vector3):ReturnType = (new Vector3(data(0), data(1), data(2))).subBy(other)   // Faster than using apply
    
    override def -(value:Double):ReturnType = (new Vector3(data(0), data(1), data(2))).subBy(value)   // Faster than using apply

    def *(other:Vector3):ReturnType = (new Vector3(data(0), data(1), data(2))).multBy(other)   // Faster than using apply
    
    override def *(value:Double):ReturnType = (new Vector3(data(0), data(1), data(2))).multBy(value)   // Faster than using apply

    def /(other:Vector3):ReturnType = (new Vector3(data(0), data(1), data(2))).divBy(other)   // Faster than using apply
    
    override def /(value:Double):ReturnType = (new Vector3(data(0), data(1), data(2))).divBy(value)   // Faster than using apply
    
    def dot(other:Vector3):Double = {
        // Much faster than original on n elements.
        val o = other.data
        data(0)*o(0) + data(1)*o(1) + data(2)*o(2)
    }

    /** Compute the angle in radians between the two vectors. */
    def angle(other:Vector3):Double = {
        val v0 = Vector3(other)
        val v1 = Vector3(this)
        v0.normalize
        v1.normalize
        math.acos(v0.dot(v1))
    }

    type ReturnType = Vector3
    
    protected[math] final val data = Array[Double](xInit, yInit, zInit)
    
    def this(other:Vector3) = this(other.x, other.y, other.z)
    
    def this() = this(0, 0, 0)
    
    def newInstance = new Vector3
    
    override final def size:Int = 3
}


//===================================================


object Vector4 {
    implicit def vector4ToTuple(v:Vector4):(Double, Double, Double, Double) = (v.x, v.y, v.z, v.w)
    def apply(x:Double, y:Double, z:Double, w:Double) = new Vector4(x, y, z, w)
    def apply() = new Vector4()
    def apply(other:NumberSeq) = {
        if(other.size < 1) // Nooooo !!!
             new Vector4()
        else if(other.size < 2)
             new Vector4(other.data(0), 0, 0, 0)
        else if(other.size < 3)
             new Vector4(other.data(0), other.data(1), 0, 0)
        else if(other.size < 4)
             new Vector4(other.data(0), other.data(1), other.data(2), 0)
        else new Vector4(other.data(0), other.data(1), other.data(2), other.data(3))
    }
    def apply(xyzw:(Double, Double, Double, Double)) = new Vector4(xyzw._1, xyzw._2, xyzw._3, xyzw._4)
    def apply(xyz:(Double, Double, Double), w:Double) = new Vector4(xyz._1, xyz._2, xyz._3, w)
    def apply(xy:(Double, Double), zw:(Double,Double)) = new Vector4(xy._1, xy._2, zw._1, zw._2)
    def apply(x:Double, yz:(Double, Double), w:Double) = new Vector4(x, yz._1, yz._2, w)
    def apply(x:Double, yzw:(Double, Double, Double)) = new Vector4(x, yzw._1, yzw._2, yzw._3)
    def apply(fill:Double) = new Vector4(fill, fill, fill, fill)
}


class Vector4(xInit:Double, yInit:Double, zInit:Double, wInit:Double) extends NumberSeq4 {
    type ReturnType = Vector4
    
    protected[math] final val data = Array[Double](xInit, yInit, zInit, wInit)
    
    def this(other:Vector4) = this(other.x, other.y, other.z, other.w)
    
    def this() = this(0, 0, 0, 0)
    
    def newInstance = new Vector4
    
    override final def size:Int = 4
}