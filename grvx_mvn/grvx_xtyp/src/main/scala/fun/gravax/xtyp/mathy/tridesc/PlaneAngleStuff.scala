package fun.gravax.xtyp.mathy.tridesc

private trait PlaneAngleStuff


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
trait UnboundedAngle[RNT <: WeakRealNum, WINT <: WeakIntegerNum[WINT]] {
	def asRadians : RNT  // may use a transcendental Pi symbol
	def asDegrees : RNT
	def getBoundedAngle : GeometricPlaneAngle[RNT]
	def getWindingNumber : SignedCountNum
}
trait GeometricPlaneAngle[RNT <: WeakRealNum] {
	def asRadians : RNT  // may use a transcendental Pi symbol
	def asDegrees : RNT
	def toUnboundedAngle[WINT <: WeakIntegerNum[WINT]](opt_windNum : Option[WINT]) : UnboundedAngle[RNT, WINT]
}

/***
 * https://en.wikipedia.org/wiki/Trigonometric_functions#Algebraic_values
 *
 * For an angle which, measured in degrees, is a multiple of three, the exact trigonometric values of the
 * sine and the cosine may be expressed in terms of square roots. These values of the sine and the cosine
 * may thus be constructed by ruler and compass.
 *
 * For an angle of an integer number of degrees, the sine and the cosine may be expressed in terms of square roots
 * and the cube root of a non-real complex number. Galois theory allows a proof that, if the angle is not a multiple
 * of 3°, non-real cube roots are unavoidable.
 *
 * For an angle which, expressed in degrees, is a rational number, the sine and the cosine are algebraic numbers,
 * which may be expressed in terms of nth roots. This results from the fact that the Galois groups of the cyclotomic
 * polynomials are cyclic.
 *
 * For an angle which, expressed in degrees, is not a rational number, then either the angle or both the sine and the
 * cosine are transcendental numbers. This is a corollary of Baker's theorem, proved in 1966.
*/