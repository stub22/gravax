package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Algebraic, Rational, Real}

private trait WeakNumSpireImplStuff

/*
Implementations using value-wrapper approach, which is becoming unfashionable.

 */
trait FromSpireNum extends ArithmeticNum {}

trait FromSpireReal extends FromSpireNum with WeakRealNum {
	val mySpireReal : Real
}

trait FromSpireRat extends FromSpireNum  with WeakRationalNum[FromSpireRat]  {
	val mySpireRat : Rational
}


private class SRtNImpl(oneSpireRat : Rational) extends FromSpireRat {
	override val mySpireRat: Rational = oneSpireRat

	override def plus(otherFSR: FromSpireRat): FromSpireRat = {
		val otherRat = otherFSR.mySpireRat
		val sumRat = mySpireRat.+(otherRat)
		new SRtNImpl(sumRat)
	}

	override def negate: FromSpireRat = ???

	override def times(otherRN: FromSpireRat): FromSpireRat = ???

	override def divide(otherRN: FromSpireRat): Option[FromSpireRat] = ???

	override def asAlgebraicNum: WeakAlgebraicNum[FromSpireRat] = ???

	override def reliableZeroCheck: Boolean = ???

	override def reliableEqualsCheck(otherEN: FromSpireRat): Boolean = ???

	override def asRealNum: WeakRealNum = ???
}
trait FromSpireAlg extends FromSpireNum with WeakAlgebraicNum[FromSpireAlg] {
	/** *
	 * https://typelevel.org/spire/guide.html#algebraic
	 * The Algebraic type is an implementation of a number for "Exact Geometric Computation".
	 * It represents algebraic numbers using an AST of the operations performed on it.
	 * Algebraic numbers can be compared accurately and exactly.
	 * This means that if we have two numbers a and b, then a compare b is always correct, regardless of whether they
	 * are irrational or incredibly close to each other. They are suitable for use in algorithms that use square-
	 * or n-roots and rely on sign tests and numeric comparison to work correctly.
	 * Because Algebraic can represent algebraic numbers (note: we are adding support for polynomial roots, not just n-roots),
	 * they have a wider range than Rational.
	 * However, whereas Rational represents numbers exactly, Algebraic can only compare exactly. They also sacrifice
	 * performance to achieve this, and so are not suitable for use where you need performance and can tolerate a
	 * certain amount of error.
	 *
	 * https://javadoc.io/doc/org.typelevel/spire_2.13/latest/spire/math/Algebraic.html
	 * Algebraic provides an exact number type for algebraic numbers.
	 * Algebraic numbers are roots of polynomials with rational coefficients.
	 * exprs = addition, multiplication, division, n-roots (eg. sqrt or cbrt), and roots of rational polynomials.
	 * Similar to Rational, but adds roots as a valid, exact operation.
	 * The cost is that this will not be as fast as Rational for many operations.
	 * In general, you can assume all operations on this number type are exact, except for those that
	 * explicitly construct approximations to an Algebraic number, such as toBigDecimal.
	 */

}
private abstract class SAlgNImpl(oneSpireAlg : Algebraic) extends FromSpireAlg {

}
object SpireBackedNumMaker {
	def mkRat(sRat : Rational) : FromSpireRat = new SRtNImpl(sRat)
}