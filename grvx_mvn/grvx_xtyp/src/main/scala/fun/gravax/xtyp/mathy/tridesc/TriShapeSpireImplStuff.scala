package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Rational, Real => SpReal}

private trait TriShapeSpireImplStuff

protected abstract class WrongConcreteTriShape(sideA : FromSpireRat, sideB : FromSpireRat, sideC : FromSpireRat ) extends TriShape {
	override type RealNT = FromSpireReal
	override type AngleTy = GeometricPlaneAngle[RealNT]
	override type LengthTy = FromSpireRat
	override type AreaTy = FromSpireAlg

}
trait WrongMkConcreteTriShapes {
	//	val aggCalc = new TriShapeSideAggCalc[FromSpireRat, FromSpireAlg, WeakCountNum ]{
	//		override lazy val myCountConsts: CountConsts[WeakCountNum] = ???
	//	}
	def makeOne(sA : FromSpireRat, sB : FromSpireRat, sC : FromSpireRat ) = new WrongConcreteTriShape(sA, sB, sC) {
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

trait WrongTSOps[TS <: TriShape] {
	def perimeter(triShp : TS) : TS#LengthTy
}

trait YesOk {
	type OurTS = TriShapeP[SpReal, SpReal, SpReal, SpReal]
	def getOurOps : WrongTSOps[OurTS]
	def pretend(ts : OurTS) : Unit = {
		val tsOps = getOurOps
		val p: SpReal = tsOps.perimeter(ts)
	}
}