package org.sofa.math

import org.sofa.nio._
import scala.math._
import scala.compat.Platform


/** Simple sequence of numbers.
  * 
  * This is the basis for vectors or any size, points or any kind or set of
  * numbers.
  * 
  * ==A note on the design of the math library==
  * 
  * The choice in designing the math library was to always use double real numbers instead of
  * creating a version of each class for floats or doubles, or using type parameters. This
  * design choice  implies that sometimes one will have to copy an array in a given format to
  * another (doubles to float).
  *
  * There exist @specialized or even @miniboxed annotation and [[Numeric]], [[Fractional]],
  * [[Intergral]] types that may help, but at a (small) cost and with an awfull syntax that,
  * furthermore, will probably change. Using double on all modern architectures (even on phones
  * or such devices) is now as fast as using floats. The only cost is therefore the one of the
  * copy to another format. This is particularly true on Android for some shaders that only
  * handle floats.
  *
  * == Operators ==
  *
  * Be careful, operators like += for example do not work like for collections, here +=
  * means arithmetic add. Be also careful with the `length` method that returns the number
  * of components in the sequence, not, for example, the cartesian length, see `norm`.
  *
  * == Optimisation ==
  *
  * The methods provided here are general, and work for any number of values inside
  * this sequence. Dedicated version for 2, 3 and 4 components, (vectors and points)
  * are provided with implementations of the methods that are optimized.
  */
trait NumberSeq extends IndexedSeq[Double] {
    
// Attribute
    
    /** The return type of operations that generate a new NumberSeq.
      *
      * As +, -, * and / must return a NumberSeq as this trait is
      * specialized as a Vector or Point, such operations should instead
      * return a Vector or a Point not a NumberSeq. This type is therefore
      * specialized in the concrete classes that use it. */
    type ReturnType <: NumberSeq
    
    /** Real content. */
    protected[math] val data:Array[Double]
 
// Access

    /** Number of elements. This is defined in SeqLike, do not confuse with norm !!. */
    def length:Int = data.length

    /** `i`-th element. */
    final def apply(i:Int):Double = data(i)
	
    /** True if all components are zero. */
	def isZero:Boolean = {
        var ok = true
        var i  = 0
        val n  = size
        while(i < n) {
            if(data(i) != 0) {
                i = n
                ok = false
            }
        }
        ok
    }
	
	/** True if all components are zero. */
	def isOrigin:Boolean = isZero

	override def toString():String = {
	    val buf = new StringBuffer
	    
	    buf.append("(")
	    buf.append(mkString(", "))
	    buf.append(")")
	    buf.toString
	}

	/** Compact string representation. */
	def toShortString():String = {
		val buf = new StringBuffer

		buf.append("(")
		var i = 0
		while(i < data.length) {
			buf.append("%+.2f".format(data(i)))
			i += 1
			if(i != data.length)
				buf.append(" ")
		}
		buf.append(")")
		buf.toString
	}
	
	/** New number sequence of the same size as this. This is not a copy of the element of this. */
	protected[math] def newInstance():ReturnType

	/** New number sequence of the same size as this with a copy of this. */
	def newClone():ReturnType = { val res = newInstance; res.copy(this); res }

// Conversion

    /** This sequence as an array of doubles. There is no convertion, since this is the native format. */
    def toDoubleArray:Array[Double] = data
    
    /** This sequence converted as an array of floats. Be careful, this may
     * return the same array several times with updated data for efficiency
     * reasons. T */
    def toFloatArray:Array[Float] = {
        val n     = data.length
        var i     = 0
        val array = new Array[Float](n)
        while(i < n) {
            array(i) = data(i).toFloat
            i += 1
        }
        array
    }
    
    /** This sequence converted as a NIO buffer of doubles. */
    def toDoubleBuffer:DoubleBuffer = DoubleBuffer(data)

    /** This sequence converted  as a NIO buffer of floats. */
    def toFloatBuffer:FloatBuffer = FloatBuffer(toFloatArray)
    
// Modification

    /** Is the size of `other` the same as this ? If not throw a `RuntimeException`. */
    final protected def checkSizes(other:NumberSeq) {
    	if(other.size != size) throw new RuntimeException("operation available on number sequences of same size only")
    }
    
    /** Assign `value` to the `i`-th element. */
    def update(i:Int, value:Double) = data(i) = value
    
    /** Copy the content of `data` in this.
      * 
      * The size of the smallest sequence determine the number of elements copied. */
    def copy(data:Traversable[Double]) {
    	data match {
    		case o:NumberSeq => {
    			Platform.arraycopy(o.data, 0, data, 0, math.min(size, o.size))		
    		}
    		case _ => {
		        val n = math.min(size, data.size) 
		        var i = 0
		        
		        data.foreach { item =>
		            if(i < n) {
		            	this.data(i) = item
		            }
		            i += 1
		        }
    		}
    	}
    }

    /** Copy the content of `other` in this.
      *
      * The size of the smallest sequence determine the number of elements copied. */
    def copy(other:NumberSeq) {
    	Platform.arraycopy(other.data, 0, data, 0, math.min(size, other.size))
    }

	/** Copy `value` in each component. */
	def fill(value:Double) {
		// if(value == 0) {
		// 	Platform.arrayclear(data)	// Works only on integers :'(
		// } else {
			val n = size
	    	var i = 0
	    	while(i < n) {
	    		data(i) = value
	    		i += 1
	    	}
//	    }
	}

	/** Add each element of `other` to the corresponding element of this.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be added, starting at 0.
	  */
	def addBy(other:NumberSeq):ReturnType = {
	    val n = math.min(size, other.size)
	    var i = 0
	    while(i < n) {
	    	data(i) += other(i)
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Add `value` to each element of this.
	  *
	  * This modifies in place this sequence.
	  */
	def addBy(value:Double):ReturnType = {
	    val n = size
	    var i = 0
	    while(i < n) {
	    	data(i) += value
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Add each element of `other` to the corresponding element of this.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be added, starting at 0.
	  */
	def +=(other:NumberSeq):ReturnType = addBy(other)

	/** Add `value` to each element of this.
	  *
	  * This modifies in place this sequence.
	  */
	def +=(value:Double):ReturnType = addBy(value)
	
	/** Result of the addition of each element of this by the corresponding element of
	  * `other`.
	  * 
	  * The two sequences must have the same size.
	  * 
	  * @return a new number sequence result of the addition.
	  */
    def +(other:NumberSeq):ReturnType = newClone.addBy(other).asInstanceOf[ReturnType]
    
    /** Result of the addition of value to each element of this.
      * 
      * @return a new number sequence result of the addition. 
      */
    def +(value:Double):ReturnType = newClone.addBy(value).asInstanceOf[ReturnType]

	/** Subtract each element of `other` to the corresponding element of this.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be added, starting at 0.
	  */
	def subBy(other:NumberSeq):ReturnType = {
	    val n = math.min(size, other.size)
	    var i = 0
	    while(i < n) {
	    	data(i) -= other(i)
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Subtract `value` to each element of this.
	  *
	  * This modifies in place this sequence.
	  */
	def subBy(value:Double):ReturnType = {
	    val n = size
	    var i = 0
	    while(i < n) {
	    	data(i) -= value
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Subtract each element of `other` to the corresponding element of this.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be added, starting at 0.
	  */
	def -=(other:NumberSeq):ReturnType = subBy(other)

	/** Subtract `value` to each element of this.
	  *
	  * This modifies in place this sequence.
	  */
	def -=(value:Double):ReturnType = subBy(value)
	
	/** Result of the subtraction of each element `other` to the corresponding element of
	  * this.
	  * 
	  * The two sequences must have the same size.
	  * 
	  * @return a new number sequence result of the subtraction.
	  */
    def -(other:NumberSeq):ReturnType = newClone.subBy(other).asInstanceOf[ReturnType]
    
    /** Result of the subtraction of value to each element of this.
      * 
      * @return a new number sequence result of the subtraction. 
      */
    def -(value:Double):ReturnType = newClone.subBy(value).asInstanceOf[ReturnType]
	
	/** Multiply each element of `other` with the corresponding element of this.
	  * 
	  * The two sequences must have the same size.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be multiplied, starting at 0.
	  */
	def multBy(other:NumberSeq):ReturnType = {
	    val n = math.min(size, other.size)
	    var i = 0
	    while(i < n) {
	    	data(i) *= other(i)
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Multiply each element of this by `value`.
	  * 
	  * This modifies in place this sequence.
	  */
	def multBy(value:Double):ReturnType = {
	    val n = size
	    var i = 0
	    while(i < n) {
	    	data(i) *= value
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Multiply each element of `other` with the corresponding element of this.
	  * 
	  * The two sequences must have the same size.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be multiplied, starting at 0.
	  */
	def *=(other:NumberSeq):ReturnType = multBy(other)

	/** Multiply each element of this by `value`.
	  * 
	  * This modifies in place this sequence.
	  */
	def *=(value:Double):ReturnType = multBy(value)
	
	/** Result of the multiplication of each element of this by the corresponding element of
	  * `other`.
	  * 
	  * The two sequences must have the same size.
	  * 
	  * @return a new number sequence result of the multiplication.
	  */
	def *(other:NumberSeq):ReturnType = newClone.multBy(other).asInstanceOf[ReturnType]
	
	/** Result of the multiplication of each element of this by `value`.
	  * 
	  * @return a new number sequence result of the multiplication.
	  */
	def *(value:Double):ReturnType = newClone.multBy(value).asInstanceOf[ReturnType]
	
	/** Divide each element of this by the corresponding element of `other`.
	  * 
	  * The two sequences must have the same size.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be divided, starting at 0.
	  */
	def divBy(other:NumberSeq):ReturnType = {
	    checkSizes(other)
	    val n = math.min(size, other.size)
	    var i = 0
	    while(i < n) {
	    	data(i) /= other(i)
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** Divide each element of this by `value`.
	  * 
	  * This modifies in place this sequence.
	  */
	def divBy(value:Double):ReturnType = {
	    val n = size
	    var i = 0
	    while(i < n) {
	    	data(i) /= value
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}
	
	/** Divide each element of this by the corresponding element of `other`.
	  * 
	  * The two sequences must have the same size.
	  *
	  * This modifies in place this sequence. The size of the smallest sequence determines the
	  * number of elements to be divided, starting at 0.
	  */
	def /=(other:NumberSeq):ReturnType = divBy(other)

	/** Divide each element of this by `value`.
	  * 
	  * This modifies in place this sequence.
	  */
	def /=(value:Double):ReturnType = divBy(value)
	
	/** Result of the division of each element of this by the corresponding element of
	  * `other`.
	  * 
	  * The two sequences must have the same size.
	  * 
	  * @return a new number sequence result of the division.
	  */
	def /(other:NumberSeq):ReturnType = newClone.divBy(other).asInstanceOf[ReturnType]

	/** Result of the division of each element of this by `value`.
	  * 
	  * @return a new number sequence result of the division.
	  */
	def /(value:Double):ReturnType = newClone.divBy(value).asInstanceOf[ReturnType]
	
	/** Dot product of this by the set of `values`.
	  * 
	  * The set of `values` must have at least the same number of components as this sequence,
	  * else the dot product is made on the minimum number of elements. 
	  */
	def dot(values:Double*):Double = {
	    val n = math.min(values.length, size)
	    var i = 0
	    var result = 0.0
	    while(i < n) {
	        result += data(i) * values(i)
	        i += 1
	    }
	    result
	}
	
	/** Dot product of this by `other`.
	  * 
	  * The two sequences must have the same size.
	  */
	def dot(other:NumberSeq):Double = {
	    checkSizes(other)
	    val n = size
	    var i = 0
		var result = 0.0
		while(i < n) {
		    result += data(i) * other.data(i)
		    i += 1
		}
		result
	}
	
	/** Dot product of `this` and `other`.
	  * 
	  * The two sequences must have the same size.
	  */
	def **(other:NumberSeq):Double = dot(other)
	
	/** Magnitude of this (length in terms of distance). */
	def norm:Double = {
		var result = 0.0
		var i = 0
		val n = size
		while(i < n) {
			result += data(i) * data(i)
			i += 1
		}
		math.sqrt(result)

	    // var result = 0.0
	    // foreach { item => result += item * item }
	    // scala.math.sqrt(result)
	}
	
	/** Multiply each element of this by the norm of this.
	  * 
	  * Changes are applied to this in place. 
	  */
	def normalize():Double = {
	    val len = norm
	    var i   = 0
	    val n   = size
	    while(i < n) {
	        data(i) /= len
	        i += 1
	    }
	    len
	}
	
	/** Result of the normalization of this. 
	  * 
	  * @return a new number sequence normalization of this.
	  * @see [[normalize]]
	  */
	def normalized():ReturnType = {
		val result = newClone
		result.normalize
	    result
	}
	
	/** Arbitrarily move this point in a random direction of a maximum given factor. */
	def brownianMotion(factor:Double) {
		var i = 0
		val n = size
		while(i < n) {
			data(i) += (math.random-0.5) * factor
			i += 1
		}
	}

	/** Store in this number seq the maximum value component-wise with `other`. */
	def maxBy(other:NumberSeq):ReturnType = {
	    val n = math.min(size, other.size)
	    var i = 0
	    while(i < n) {
	    	if(data(i) < other(i))
	    		data(i) = other(i)
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}
	
	/** Store in this number seq the minimum value component-wise with `other`. */
	def minBy(other:NumberSeq):ReturnType = {
	    val n = math.min(size, other.size)
	    var i = 0
	    while(i<n) {
	    	if(data(i) > other(i))
	    		data(i) = other(i)
	    	i += 1
	    }
	    this.asInstanceOf[ReturnType]
	}

	/** True if this point coordinates are between `from` and `to`. This is a bounding box inclusion.
	  * The point is considered inside even if coordinates are equal. This means that >= and <= operations
	  * are used, in other words with this point equal to `from` and `to`, the operation returns true. */
	def inside(from:NumberSeq, to:NumberSeq):Boolean = {
		var inside = true
		var dim = 0
		val n = math.min(data.length, math.min(from.data.length, to.data.length))
		
		while(dim < n && inside) {
			inside = (data(dim)>= from.data(dim) && data(dim) <= to.data(dim))
			dim += 1
		}

		inside
	}

	/** True if this point coordinates are between points `first` and `second` even if
	  * the point coordinates of `second` are not >= to `first`. This is a bounding box inclusion.
	  * The `first` and `second` point coordinates are sorted to test the inclusion.
	  * The point is considered inside even if coordinates are equal. This means that >= and <= operations
	  * are used, in other words with this point equal to `from` and `to`, the operation returns true. */
	def insideOneOrAnother(first:NumberSeq, second:NumberSeq):Boolean = {
		var inside = true
		var dim = 0
		val n = math.min(data.length, math.min(first.data.length, second.data.length))
		
		while(dim < n && inside) {
			inside = (data(dim) >= math.min(first.data(dim), second.data(dim)) &&
				      data(dim) <= math.max(first.data(dim), second.data(dim)))
			dim += 1
		}

		inside
	}
}


//===================================================


trait NumberSeq2 extends NumberSeq {
    final def x:Double = data(0)
    final def y:Double = data(1)
    final def xy:(Double, Double) = (data(0), data(1))
    
    final def x_=(value:Double) = data(0) = value
    final def y_=(value:Double) = data(1) = value
    final def xy_=(value:(Double, Double)) = { data(0) = value._1; data(1) = value._2 }
    
    final def width:Double = data(0)
    final def height:Double = data(1)

    final def width_=(value:Double) = data(0) = value
    final def height_=(value:Double) = data(1) = value

    def set(x:Double, y:Double):ReturnType = { data(0) = x; data(1) = y; this.asInstanceOf[ReturnType] }
    
    def copy(other:NumberSeq2) {
        // Much faster than original on n elements.
        val o = other.data
        data(0) = o(0)
        data(1) = o(1)
    }

    override def norm:Double = {
        // Much faster than original on n elements.
        math.sqrt(data(0)*data(0) + data(1)*data(1))
    }
    
    override def normalize():Double = {
        // Much faster than original on n elements.
        val len = norm
        data(0) /= len
        data(1) /= len
        len
    }

    def +=(other:NumberSeq2):ReturnType = addBy(other)

    override def +=(value:Double):ReturnType = addBy(value)

    def addBy(other:NumberSeq2):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) += o(0)
        data(1) += o(1)
        this.asInstanceOf[ReturnType]
    }

    override def addBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) += value
        data(1) += value
        this.asInstanceOf[ReturnType]
    }

    def -=(other:NumberSeq2):ReturnType = subBy(other)

    override def -=(value:Double):ReturnType = subBy(value)

    def subBy(other:NumberSeq2):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) -= o(0)
        data(1) -= o(1)
        this.asInstanceOf[ReturnType]
    }

    override def subBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) -= value
        data(1) -= value
        this.asInstanceOf[ReturnType]
    }

    def *=(other:NumberSeq2):ReturnType = multBy(other)

    override def *=(value:Double):ReturnType = multBy(value)

    def multBy(other:NumberSeq2):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) *= o(0)
        data(1) *= o(1)
        this.asInstanceOf[ReturnType]
    }

    override def multBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) *= value
        data(1) *= value
        this.asInstanceOf[ReturnType]
    }

    def /=(other:NumberSeq2):ReturnType = divBy(other)

    override def /=(value:Double):ReturnType = divBy(value)

    def divBy(other:NumberSeq2):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) /= o(0)
        data(1) /= o(1)
        this.asInstanceOf[ReturnType]
    }

    override def divBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) /= value
        data(1) /= value
        this.asInstanceOf[ReturnType]
    }
}


//===================================================


trait NumberSeq3 extends NumberSeq2 {
	final def z:Double = data(2)
    final def yz:(Double, Double) = (data(1), data(2))
    final def xz:(Double, Double) = (data(0), data(2))
    final def xyz:(Double, Double, Double) = (data(0), data(1), data(2))
    
    final def z_=(value:Double) = data(2) = value
    final def yz_=(value:(Double, Double)) = { data(1) = value._1; data(2) = value._2 }
    final def xz_=(value:(Double, Double)) = { data(0) = value._1; data(2) = value._2 }
    final def xyz_=(value:(Double, Double, Double)) = { data(0) = value._1; data(1) = value._2; data(2) = value._3 }
    
    final def depth:Double = data(2)

    final def depth_=(value:Double) = data(2) = value

    def set(x:Double, y:Double, z:Double):ReturnType = { data(0) = x; data(1) = y; data(2) = z; this.asInstanceOf[ReturnType] }
        
    def copy(other:NumberSeq3) {
        // Much faster than original on n elements.
        val o = other.data
        data(0) = o(0)
        data(1) = o(1)
        data(2) = o(2)
    }

    override def norm:Double = {
        // Much faster than original on n elements.
        math.sqrt(data(0)*data(0) + data(1)*data(1) + data(2)*data(2))
    }
    
    override def normalize():Double = {
        // Much faster than original on n elements.
        val len = norm
        data(0) /= len
        data(1) /= len
        data(2) /= len
        len
    }

    def +=(other:NumberSeq3):ReturnType = addBy(other)

    override def +=(value:Double):ReturnType = addBy(value)

    def addBy(other:NumberSeq3):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) += o(0)
        data(1) += o(1)
        data(2) += o(2)
        this.asInstanceOf[ReturnType]
    }

    override def addBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) += value
        data(1) += value
        data(2) += value
        this.asInstanceOf[ReturnType]
    }

    def -=(other:NumberSeq3):ReturnType = subBy(other)

    override def -=(value:Double):ReturnType = subBy(value)

    def subBy(other:NumberSeq3):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) -= o(0)
        data(1) -= o(1)
        data(2) -= o(2)
        this.asInstanceOf[ReturnType]
    }

    override def subBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) -= value
        data(1) -= value
        data(2) -= value
        this.asInstanceOf[ReturnType]
    }

    def *=(other:NumberSeq3):ReturnType = multBy(other)

    override def *=(value:Double):ReturnType = multBy(value)

    def multBy(other:NumberSeq3):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) *= o(0)
        data(1) *= o(1)
        data(2) *= o(2)
        this.asInstanceOf[ReturnType]
    }

    override def multBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) *= value
        data(1) *= value
        data(2) *= value
        this.asInstanceOf[ReturnType]
    }

    def /=(other:NumberSeq3):ReturnType = divBy(other)

    override def /=(value:Double):ReturnType = divBy(value)

    def divBy(other:NumberSeq3):ReturnType = {
        // Much faster than original on n elements.
        val o = other.data
        data(0) /= o(0)
        data(1) /= o(1)
        data(2) /= o(2)
        this.asInstanceOf[ReturnType]
    }

    override def divBy(value:Double):ReturnType = {
        // Much faster than original on n elements.
        data(0) /= value
        data(1) /= value
        data(2) /= value
        this.asInstanceOf[ReturnType]
    }
}


//===================================================


trait NumberSeq4 extends NumberSeq3 {
	final def w:Double = data(3)
    final def xw:(Double, Double) = (data(0), data(3))
    final def yw:(Double, Double) = (data(1), data(3))
    final def zw:(Double, Double) = (data(2), data(3))
    final def xyw:(Double, Double, Double) = (data(0), data(1), data(3))
    final def xzw:(Double, Double, Double) = (data(0), data(2), data(3))
    final def yzw:(Double, Double, Double) = (data(1), data(2), data(3))
    final def xyzw:(Double, Double, Double, Double) = (data(0), data(1), data(2), data(3))
    
    final def w_=(value:Double) = data(3) = value
    final def xw_=(value:(Double, Double)) = { data(0) = value._1; data(3) = value._2 }
    final def yw_=(value:(Double, Double)) = { data(1) = value._1; data(3) = value._2 }
    final def zw_=(value:(Double, Double)) = { data(2) = value._1; data(3) = value._2 }
    final def xyw_=(value:(Double, Double, Double)) = { data(0) = value._1; data(1) = value._2; data(3) = value._3 }
    final def xzw_=(value:(Double, Double, Double)) = { data(0) = value._1; data(2) = value._2; data(3) = value._3 }
    final def yzw_=(value:(Double, Double, Double)) = { data(1) = value._1; data(2) = value._2; data(3) = value._3 }
    final def xyzw_=(value:(Double, Double, Double, Double)) = { data(0) = value._1; data(1) = value._2; data(2) = value._3; data(3) = value._4 }

    def set(x:Double, y:Double, z:Double, w:Double):ReturnType = { data(0) = x; data(1) = y; data(2) = z; data(3) = w; this.asInstanceOf[ReturnType] }
}


//===================================================


trait ConstantNumberSeq extends NumberSeq {
	protected def error():ReturnType = { throw new RuntimeException("immutable Point3") }

	override def update(i:Int, value:Double)          { error }
	override def copy(data:Traversable[Double])       { error }
    override def copy(other:NumberSeq)                { error }
	override def fill(value:Double)                   { error }
	override def addBy(other:NumberSeq):ReturnType  = { error }
	override def addBy(value:Double):ReturnType     = { error }
	override def subBy(other:NumberSeq):ReturnType  = { error }
	override def subBy(value:Double):ReturnType     = { error }
	override def multBy(other:NumberSeq):ReturnType = { error }
	override def multBy(value:Double):ReturnType    = { error }
	override def divBy(other:NumberSeq):ReturnType  = { error }
	override def divBy(value:Double):ReturnType     = { error }
	override def normalize():Double                 = { error; return 0.0 }
	override def brownianMotion(factor:Double)        { error }
	override def maxBy(other:NumberSeq):ReturnType  = { error }
	override def minBy(other:NumberSeq):ReturnType  = { error }
}


trait ConstantNumberSeq2 extends NumberSeq2 with ConstantNumberSeq {
    override def set(x:Double, y:Double):ReturnType  = { error }
    override def copy(other:NumberSeq2)                { error }
    override def addBy(other:NumberSeq2):ReturnType  = { error }
    override def subBy(other:NumberSeq2):ReturnType  = { error }
    override def multBy(other:NumberSeq2):ReturnType = { error }
    override def divBy(other:NumberSeq2):ReturnType  = { error }
}


trait ConstantNumberSeq3 extends NumberSeq3 with ConstantNumberSeq {
    override def set(x:Double, y:Double, z:Double):ReturnType = { error }
    override def copy(other:NumberSeq3)                         { error }
    override def addBy(other:NumberSeq3):ReturnType           = { error }
    override def subBy(other:NumberSeq3):ReturnType           = { error }
    override def multBy(other:NumberSeq3):ReturnType          = { error }
    override def divBy(other:NumberSeq3):ReturnType           = { error }
}


trait ConstantNumberSeq4 extends NumberSeq4 with ConstantNumberSeq {
    override def set(x:Double, y:Double, z:Double, w:Double):ReturnType = { error }
    // override def copy(other:NumberSeq4)                                   { error }
    // override def addBy(other:NumberSeq4):ReturnType                     = { error }
    // override def subBy(other:NumberSeq4):ReturnType                     = { error }
    // override def multBy(other:NumberSeq4):ReturnType                    = { error }
    // override def divBy(other:NumberSeq4):ReturnType                     = { error }
}
