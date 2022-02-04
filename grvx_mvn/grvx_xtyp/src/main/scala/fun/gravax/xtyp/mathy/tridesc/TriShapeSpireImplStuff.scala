package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Rational, Real}

private trait TriShapeSpireImplStuff
/*
trait RealAngledTriShape extends TriShape {
	override type RealNT = FromSpireNum
	override type AngleTy = GeometricPlaneAngle[RealNT]
}
trait RatlenAlgangTriShape extends RealAngledTriShape {
	override type LengthTy = FromSpireRat
	override type AreaTy <: FromSpireAlg[AreaTy]
	// cyclic reference not allowed in   = FromSpireAlg[AreaTy]
	// PlainAlgebraic
}

case class BadTriShape(a : Int, b : Int, c : Int) extends RatlenAlgangTriShape {

	override def sidesIncreasing: (LengthTy, LengthTy, LengthTy) = ???

	override def anglesIncreasng: (AngleTy, AngleTy, AngleTy) = ???

	override def perimeter: LengthTy = ???

	override def area: AreaTy = ???
}
*/