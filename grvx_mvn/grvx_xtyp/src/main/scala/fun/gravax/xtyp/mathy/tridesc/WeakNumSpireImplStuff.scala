package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Rational, Real}

private trait WeakNumSpireImplStuff


trait FromSpireReal extends WeakRealNum {
	final override type MyArgTy = FromSpireReal
	final override type MyRsltTy = FromSpireReal
	val mySpireReal : Real
}
private class SRlNImpl(oneSpireReal : Real) extends FromSpireReal {
	override val mySpireReal: Real = oneSpireReal

	override def plus(otherRN: FromSpireReal): FromSpireReal = ???

	override def negate: FromSpireReal = ???

	override def times(otherRN: FromSpireReal): FromSpireReal = ???

	override def divide(otherRN: FromSpireReal): Option[FromSpireReal] = ???

	override def fastWeakEqualsCheck(otherRN: FromSpireReal): Option[Boolean] = ???

	override def fastWeakZeroCheck: Option[Boolean] = ???
}
trait FromSpireRat extends WeakRealNum {
	override type MyArgTy = FromSpireRat
	override type MyRsltTy = FromSpireRat
	val mySpireRat : Rational
}
private class SRtNImpl(oneSpireRat : Rational) extends FromSpireRat {
	override val mySpireRat: Rational = mySpireRat

	override def plus(otherRN: FromSpireRat): FromSpireRat = ???

	override def negate: FromSpireRat = ???

	override def times(otherRN: FromSpireRat): FromSpireRat = ???

	override def divide(otherRN: FromSpireRat): Option[FromSpireRat] = ???

	override def fastWeakEqualsCheck(otherRN: FromSpireRat): Option[Boolean] = ???

	override def fastWeakZeroCheck: Option[Boolean] = ???
}
