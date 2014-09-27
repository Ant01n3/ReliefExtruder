package org.sofa.math


object Triangle {
	def apply(p0:Point3, p1:Point3, p2:Point3):Triangle = new Triangle(p0, p1, p2)
	def unapply(t:Triangle):(Point3,Point3,Point3) = (Point3(t.p0),Point3(t.p1),Point3(t.p2))
}


object ConstTriangle {
	def apply(p0:Point3, p1:Point3, p2:Point3):ConstTriangle = new ConstTriangle(p0, p1, p2)
	def unapply(t:ConstTriangle):(Point3,Point3,Point3) = (Point3(t.p0),Point3(t.p1),Point3(t.p2))	
}


/** A simple triangle class that allows to easily compute
  * a normal form, a normal and a distance from the triangle, with the calculus of
  * the nearest point on the triangle surface.
  *
  * This class is made for triangles whose points can be changed at any time. If
  * the triangle is not to be modified, you can improve performance by using the
  * ConstTriangle class that will pre-compute the normal and the normal form, and
  * avoid calculus during collision and distance tests. */
class Triangle(val p0:Point3, val p1:Point3, val p2:Point3) {

	/** Computes the normal form of a triangle, that is a base point,
	  * and two vectors to locate the two other points from the base point. */
	def normalForm():(Point3,Vector3,Vector3) = { (Point3(p0),Vector3(p0, p1),Vector3(p0, p2)) }

	/** Compute the triangle normal. */
	def normal():Vector3 = {
		val v0 = Vector3(p0, p1)
		val v1 = Vector3(p0, p2)
			
		val normal = v1 X v0
		normal.normalize
		normal
	}
	
	/** Compute the distance from the given point `p` to this triangle.
	  * Return the distance, and a point on the triangle where the distance
	  * is minimum to `p`.
	  * Based on David Eberly algorithm (http:\\www.geometrictools.com/Documentation/DistancePoint3Triangle3.pdf)
	  * and Gwendolyn Fisher mathlab implementation. */
	def distanceFrom(pp:Point3):(Double,Point3) = {
		val (bb,e0,e1) = normalForm()
		var dist = 0.0
		//var pp0:Point3 = null
		
		val dd = Vector3(pp, bb)
		val a = e0 dot e0
		val b = e0 dot e1
		val c = e1 dot e1
		val d = e0 dot dd
		val e = e1 dot dd
		val f = dd dot dd
		
		val det = a*c - b*b
		var s   = b*e - c*d
		var t   = b*d - a*e
		
		var sqrDistance = 0.0

		if((s+t) <= det) {
			if(s < 0) {
				if(t < 0) {
					// region4
					if(d < 0)  {
						t = 0
						if (-d >= a) {
							s = 1
							sqrDistance = a + 2*d + f
						} else {
							s = -d/a
							sqrDistance = d*s + f
						}
					} else {
						s = 0
						if (e >= 0) {
							t = 0
							sqrDistance = f
						} else {
							if (-e >= c) {
								t = 1
								sqrDistance = c + 2*e + f
							} else {
								t = -e/c
								sqrDistance = e*t + f
							}
						}
					} // of region 4
				} else {
					// region 3
					s = 0
					if(e >= 0) {
						t = 0
						sqrDistance = f
					} else {
						if(-e >= c) {
							t = 1
							sqrDistance = c + 2*e +f
						} else {
							t = -e/c
							sqrDistance = e*t + f
						}
					}
				} // of region 3 
			} else {
				if(t < 0) {
					// region 5
					t = 0
					if(d >= 0) {
						s = 0
						sqrDistance = f
					} else{
						if(-d >= a) {
							s = 1
							sqrDistance = a + 2*d + f // GF 20101013 fixed typo d*s ->2*d
						} else {
							s = -d/a
							sqrDistance = d*s + f
						}
					}
				} else {
					// region 0
					val invDet = 1/det
					s = s*invDet
					t = t*invDet
					sqrDistance = s*(a*s + b*t + 2*d) + t*(b*s + c*t + 2*e) + f
				} 
			}
		} else {
			if(s < 0) {
				// region 2
				val tmp0 = b + d
				val tmp1 = c + e
				if(tmp1 > tmp0) { // minimum on edge s+t=1
					val numer = tmp1 - tmp0;
					val denom = a - 2*b + c
					if(numer >= denom) {
						s = 1;
						t = 0;
						sqrDistance = a + 2*d + f // GF 20101014 fixed typo 2*b -> 2*d
					} else {
						s = numer/denom
						t = 1-s;
						sqrDistance = s*(a*s + b*t + 2*d) + t*(b*s + c*t + 2*e) + f
					} 
				} else {          // minimum on edge s=0
					s = 0
					if(tmp1 <= 0) {
						t = 1
						sqrDistance = c + 2*e + f
					}else{
						if(e >= 0){
							t = 0
							sqrDistance = f
						}else{
							t = -e/c
							sqrDistance = e*t + f
						}
					}
				} // of region 2
			} else {
				if(t < 0) {
					//region6 
					val tmp0 = b + e
					val tmp1 = a + d
					if (tmp1 > tmp0) {
						val numer = tmp1 - tmp0
						val denom = a-2*b+c
						if (numer >= denom) {
							t = 1
							s = 0
							sqrDistance = c + 2*e + f
						} else {
							t = numer/denom
							s = 1 - t
							sqrDistance = s*(a*s + b*t + 2*d) + t*(b*s + c*t + 2*e) + f
						}
					} else {  
						t = 0;
						if (tmp1 <= 0) {
							s = 1
							sqrDistance = a + 2*d + f
						} else {
							if (d >= 0) {
								s = 0
								sqrDistance = f
							} else {
								s = -d/a
								sqrDistance = d*s + f
							}
						}
					}
					//end region 6
				} else {
					// region 1
					val numer = c + e - b - d
					if(numer <= 0) {
						s = 0
						t = 1
						sqrDistance = c + 2*e + f
					}else {
						val denom = a - 2*b + c
						if(numer >= denom) {
							s = 1
							t = 0
							sqrDistance = a + 2*d + f
						} else {
							s = numer/denom
							t = 1-s
							sqrDistance = s*(a*s + b*t + 2*d) + t*(b*s + c*t + 2*e) + f
						}
					} // of region 1
				}
			}
		}

		// account for numerical round-off error
		if (sqrDistance < 0) {
			sqrDistance = 0
		}

		dist = math.sqrt(sqrDistance)

		// pp0 = (bb + e0*s + e1*t).asInstanceOf[Point3]
		// Optimization to avoid two point3 creation.
		var pp0 = Point3(bb)

		e0  *= s
		e1  *= t
		pp0 += e0
		pp0 += e1

		(dist,pp0)
	}
}

/** A triangle that will not be moved.
  * 
  * The use of such a triangle allows to ensure we can compute the normal to the triangle and the
  * normal form of the triangle once and forall. Then the computation of distanceFrom is far more
  * efficient. As such triangles are often used in collision tests, this can greatly improve things. */
class ConstTriangle(p0:Point3, p1:Point3, p2:Point3) extends Triangle(p0, p1, p2) {

	/** Vector between point 0 and 1. */
	val v0 = Vector3(p0, p1)

	/** Vector between point 0 and 2. */
	val v1 = Vector3(p0, p2)

	/** The normal to the triangle face. */
	protected[this] val v2 = (v1 X v0)

	v2.normalize

	/** The normal form of a triangle, that is a base point,
	  * and two vectors to locate the two other points from the base point. */
	override def normalForm():(Point3,Vector3,Vector3) = { (Point3(p0),Vector3(v0),Vector3(v1)) }

	/** Triangle normal. */
	override def normal():Vector3 = v2
}