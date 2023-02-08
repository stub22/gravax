package fun.gravax.xtyp.mathy.tridesc

private trait TriShapeDescStuff
/***
Let's "Tri" again!

A TriShape is a fully realized (euclidean plane) shape with measure.
It has side-lengths, angles, perimeter, and area, which are all (exact) Real numbers.
A TriShape must be able to supply these numbers in a form suitable for calculation.
 */

/***
The 8 numbers of the triangle are related by numerous equations.
A TriShape must satisfy all these equations, or else it should not be constructable.
Relations involving angles depend on trigonometric functions.
 */

trait TriShape {
	// These types are separated as factors for definition of medium-strong subtypes of TriShape.

	// The BOUNDS on these types are coming from the taxonomy-like "Weak" hierarchy which is questionable as
	// a computational structure, but certainly helpful as an information conveyance.

	// These 4 type-members is a type-parameterization of the space,
	// comparable to TriShape[RealNT, AngleTy, LengthTy, AreaTy], but with a different contract for the
	// client.
	// The RealNT represents the kind of real number that calculations involving the other numbers must return
	// a (logical) subtype of.  Hence it is the unification point for calculations.
	type	RealNT // <: WeakRealNum
	type 	AngleTy // <: GeometricPlaneAngle[RealNT]
	type	LengthTy // <:  ArithmeticNum
	type	AreaTy // <: ArithmeticNum
	// These 8 numbers are called the "shape numbers" of the single, concrete triangle-shape.
	// An implementor supplies these number results based on it's chosen representation.
	// The results as ordered tuples allows for both type-safety and efficiency of chained computations.

	def sidesIncreasing : (LengthTy, LengthTy, LengthTy)
	def anglesIncreasng : (AngleTy, AngleTy, AngleTy)
	def perimeter : LengthTy
	def area : AreaTy

	// With the constituent types hidden our TriShapeRec is not very powerful from client's perspective.
	// Client will have trouble directly calling our methods without knowing our types.


	// Alternative approach could be to define separate TriShapeOps with parametric poly
}
trait TriShapeP[RN, ANG, LEN, AR] extends TriShape {
	// Using our params, the client has direct access to the number types.
	override type RealNT = RN
	override type AngleTy = ANG
	override type LengthTy = LEN
	override type AreaTy = AR
}

trait TriShapeComparator {
	def fastWeakCongruenceCheck(tsA : TriShape, tsB : TriShape) : Option[Boolean]
	def fastWeakSimilarityCheck(tsA : TriShape, tsB : TriShape) : Option[Boolean]
}

trait CountConsts[CountTy] {
	val num_2 : CountTy
}
trait TranscendConsts[TrnscndTy] {
	val num_pi : TrnscndTy
	val num_twoPi : TrnscndTy
	val num_halfPi : TrnscndTy
	val num_quartPi : TrnscndTy
	val num_thirdOfPi : TrnscndTy

	val num_e : TrnscndTy
}
// Here we are saying that the LengthTy is a type variable, which is at least an algebraic number.
// The self-reference in parameter allows the result of a.plus(b) to be of LengthTy.
private trait WrongTriShapeSideAggCalc[LengthTy <: WeakAlgebraicNum[LengthTy], AreaTy <: WeakAlgebraicNum[AreaTy], CountTy <: WeakCountNum] {
	val myCountConsts : CountConsts[CountTy]

	def calcPerimFromValidTriSides(sideA : LengthTy, sideB : LengthTy, sideC : LengthTy) : LengthTy = {
		// sideA.plus(sideB).plus(sideC)
		val hmm: LengthTy = sideA.plus(sideB)
		???
	}
	// The input lenghts must satisfy triangle inequality (or be on boundary, implying shape.area==0)
	def calcAreaFromPerimAndTwoSides(perim: LengthTy, sideA : LengthTy, sideB : LengthTy) : AreaTy = {
		val numTwo: CountTy = myCountConsts.num_2
		val promotedNumTwo = numTwo
		// TODO:  Make the ".get" unnecessary by introducing a narrowing of the ".divide" operator
		// val semiPerim = perim.divide(numTwo).get
		???
	}

}

private trait WrongParamTriShape[LengthTy <: WeakAlgebraicNum[LengthTy], AreaTy <: WeakAlgebraicNum[AreaTy], CountTy <: WeakCountNum]
extends TriShape {

}