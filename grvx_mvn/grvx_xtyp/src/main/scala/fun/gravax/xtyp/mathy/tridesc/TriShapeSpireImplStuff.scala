package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Rational, Real}

private trait TriShapeSpireImplStuff

abstract class ConcreteTriShape(sideA : FromSpireRat, sideB : FromSpireRat, sideC : FromSpireRat ) extends TriShape {
	override type RealNT = FromSpireReal
	override type AngleTy = GeometricPlaneAngle[RealNT]
	override type LengthTy = FromSpireRat
	override type AreaTy = FromSpireAlg

}
trait MkConcreteTriShapes {
	//	val aggCalc = new TriShapeSideAggCalc[FromSpireRat, FromSpireAlg, WeakCountNum ]{
	//		override lazy val myCountConsts: CountConsts[WeakCountNum] = ???
	//	}
	def makeOne(sA : FromSpireRat, sB : FromSpireRat, sC : FromSpireRat ) = new ConcreteTriShape(sA, sB, sC) {
		override def sidesIncreasing: (LengthTy, LengthTy, LengthTy) = ???

		override def anglesIncreasng: (AngleTy, AngleTy, AngleTy) = ???

		override def perimeter: LengthTy = ???

		override def area: AreaTy = ???

	}
	def makeTriFromSpiRats(sprtA : Rational, sprtB : Rational, sprtC: Rational) : TriShape = {
		// TODO:  Order the sides
		val fsA = SpireBackedNumMaker.mkRat(sprtA)
		val fsB = SpireBackedNumMaker.mkRat(sprtB)
		val fsC = SpireBackedNumMaker.mkRat(sprtC)
		makeOne(fsA, fsB, fsC)
	}
}
