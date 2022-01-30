package fun.gravax.xtyp.mathy.tridesc

private trait TriShapeDescStuff
/***
Let's "Tri" again!

A TriShape is a fully realized (euclidean plane) shape with measure.
It has side-lengths, angles, perimeter, and area, which are all (exact) Real numbers.
A TriShape must be able to supply these numbers in a form suitable for calculation.
 */

/***
 * RNT and CNT bounds must suffice for all numeric ops, and are not intended as logical boundaries.
 * These type-params are for plugging in to whatever framework implementor wants, e.g. Spire.
 *
 * UnboundedAngle is equiv to cartesian pair (BoundedAngle, WindingNumber)
 * An unbounded angle may measure to any real number (in radians or degrees), positive or negative.
 * It implicitly includes a winding number.
 * A bounded angle is limited to a single range winding quantity necessarily must support radian and degree units.
 * It must be able to supply
 */
trait UnboundedAngle[RNT <: WeakRealNum, CNT <: CountNum] {
	def asRadians : RNT  // may use a transcendental Pi symbol
	def asDegrees : RNT
	def getBoundedAngle : GeometricPlaneAngle[RNT]
	def getWindingNumber : SignedCountNum
}
trait GeometricPlaneAngle[RNT <: WeakRealNum] {
	def asRadians : RNT  // may use a transcendental Pi symbol
	def asDegrees : RNT
	def toUnboundedAngle[CNT <: CountNum](opt_windNum : Option[CNT]) : UnboundedAngle[RNT, CNT]
}

/***
The 8 numbers of the triangle are related by numerous equations.
A TriShape must satisfy all these equations, or else it should not be constructable.
Relations involving angles depend on trigonometric functions.

https://en.wikipedia.org/wiki/Trigonometric_functions#Algebraic_values

For an angle which, measured in degrees, is a multiple of three, the exact trigonometric values of the
sine and the cosine may be expressed in terms of square roots. These values of the sine and the cosine
may thus be constructed by ruler and compass.

For an angle of an integer number of degrees, the sine and the cosine may be expressed in terms of square roots
and the cube root of a non-real complex number. Galois theory allows a proof that, if the angle is not a multiple
of 3°, non-real cube roots are unavoidable.

For an angle which, expressed in degrees, is a rational number, the sine and the cosine are algebraic numbers,
which may be expressed in terms of nth roots. This results from the fact that the Galois groups of the cyclotomic
polynomials are cyclic.

For an angle which, expressed in degrees, is not a rational number, then either the angle or both the sine and the
cosine are transcendental numbers. This is a corollary of Baker's theorem, proved in 1966.
 */
trait TriShape {
	// These types are separated as factors for definition of medium-strong subtypes of TriShape
	type	RealNT <: WeakRealNum
	type 	AngleTy <: GeometricPlaneAngle[RealNT]
	type	LengthTy <: RealNT
	type	AreaTy <: RealNT
	// These 8 numbers are called the "shape numbers" of the single, concrete triangle-shape.
	// An implementor supplies these number resultss based on it's chosen representation.
	// The results as ordered tuples allows for both type-safety and efficiency of chained computations
	def sidesIncreasing : (LengthTy, LengthTy, LengthTy)
	def anglesIncreasng : (AngleTy, AngleTy, AngleTy)
	def perimeter : LengthTy
	def area : AreaTy
}

trait TriShapeComparator {
	def fastWeakCongruenceCheck(tsA : TriShape, tsB : TriShape) : Option[Boolean]
	def fastWeakSimilarityCheck(tsA : TriShape, tsB : TriShape) : Option[Boolean]
}

