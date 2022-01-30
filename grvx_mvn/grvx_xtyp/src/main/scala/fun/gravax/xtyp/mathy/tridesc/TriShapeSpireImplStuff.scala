package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Rational, Real}

private trait TriShapeSpireImplStuff

trait RealTriShape extends TriShape {
	override type RealNT = FromSpireReal
	override type AngleTy = GeometricPlaneAngle[RealNT]
}
trait RatlenRealTriShape extends RealTriShape {

}

case class BadTriShape(a : Int, b : Int, c : Int) extends RatlenRealTriShape {

	override type LengthTy = RealNT
	override type AreaTy = RealNT

	override def sidesIncreasing: (LengthTy, LengthTy, LengthTy) = ???

	override def anglesIncreasng: (AngleTy, AngleTy, AngleTy) = ???

	override def perimeter: LengthTy = ???

	override def area: AreaTy = ???
}
