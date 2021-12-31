package fun.gravax.gravnum

private trait PureNumSymStuff

trait GoodNum

// These Interface traits establish interoperability for basic tests and arithmetic.
// These ops are not quite proven safe, but they are at least in a functional and somewhat inductive pattern.
// Note that all methods take 0 or 1 args, and all args+results are PN, Boolean, or Option.

// Implementation relies on platform plugin to supply a very limited number of features, primarily based
// on arithmetic over positive integers (i.e. naturals)

/*
When all methods take zero or one args
and the type constructors are all monadic
 */

trait UnaryInquiryPN {
	val isZeroPN : Boolean
	val isPositivePN : Boolean
	val isNegativePN : Boolean
	val isIntegerPN : Boolean
	val isPosIntPN : Boolean
	val isNegIntPN : Boolean
}
trait UnaryArithPN {
	def safeReciprocalPN : Option[PureNum]
	def negatePN : PureNum
	def reduceFractionPN : PureNum
}
trait BinaryInquiryPN {
	// All three methods must be given by implementor, due to varying perspectives on how to best answer these
	def isEqPN(otherPN : PureNum) : Boolean
	def isGtPN(otherPN : PureNum) : Boolean
	def isGteqPN(otherPN : PureNum) : Boolean
	// Less-than forms defined by negation
	def isLtPN(otherPN : PureNum) : Boolean // = !isGteqPN(otherPN)
	def isLteqPN(otherPN : PureNum) : Boolean // = !isGtPN(otherPN)
}
trait CommutArithPN {
	def plusPN(otherPN : PureNum) : PureNum
	def timesPN(otherPN : PureNum) : PureNum
}
trait NoncommutArithPN  {
	def minusPN(otherPN : PureNum) : PureNum
	def safelyDivideByPN(otherPN : PureNum) : Option[PureNum]
}
// For users, PureNums are not guaranteed unique in any way, but all inquiry and arith should always work correctly,
// and execute eagerly (to produce strong-typed result as data) to ensure fail-fast.  However fractions may be left
// unreduced in the results.
// Note that the result type for is the broad computational type PureNum, rather than a more restrictive
// (hence more powerful) type.  More restrictive (hence powerful) API may be offered by specific subtypes.
sealed trait PureNum extends UnaryInquiryPN with BinaryInquiryPN with UnaryArithPN with CommutArithPN with NoncommutArithPN {
}
abstract class BasePureNum extends PureNum {
	override def isLtPN(otherPN: PureNum): Boolean = !isGteqPN(otherPN)

	override def isLteqPN(otherPN: PureNum): Boolean = !isGtPN(otherPN)

	override def minusPN(otherPN: PureNum): PureNum = {
		val negOth = otherPN.negatePN
		plusPN(negOth)
	}
	override def safelyDivideByPN(otherPN: PureNum): Option[PureNum] = {
		val recipOth = otherPN.safeReciprocalPN
		// Fraction gets reduced by default, but this is not specified by API, yet.
		recipOth.map(ro => this.timesPN(ro).reduceFractionPN)
	}
}
trait ZeroPN extends PureNum {
	override val isZeroPN: Boolean = true
	override val isPositivePN: Boolean = false
	override val isNegativePN: Boolean = false
}
trait NonzeroPN extends PureNum {
	override val isZeroPN: Boolean = false
}
trait PositivePN extends NonzeroPN {
	override val isPositivePN: Boolean = true
	override val isNegativePN: Boolean = false
}
trait NegativePN extends NonzeroPN {
	override val isPositivePN: Boolean = false
	override val isNegativePN: Boolean = true
}
// Elements form an abelian additive group (with inverse), also closed under times,
// thus yielding algebraic ring of IntegerPNs, which are embedded in the algebraic field of RationalPNs.
trait IntegerPN extends PureNum with YaflIntNum {
	override val isIntegerPN: Boolean = true
	def plusIPN(otherIPN : IntegerPN) : IntegerPN
	def timesIPN(otherIPN : IntegerPN) : IntegerPN
}

trait WholeIntPN extends IntegerPN {
	def plusWIPN(otherWIPN : WholeIntPN) : WholeIntPN
	def timesWIPN (otherWIPN : WholeIntPN) : WholeIntPN
}
trait PosIntPN extends WholeIntPN with PositivePN {
	// Not a sub-ring of WholeIntPN, because no additive identity

	// Positive integer corresponds to a constructible natural number
	def plusPIPN (otherPIPN : PosIntPN) : PosIntPN
	def timesPIPN (otherPIPN : PosIntPN) : PosIntPN

	override def plusWIPN(otherWIPN: WholeIntPN): WholeIntPN = {
		if (otherWIPN.isZeroPN) this
		else {
			val posOther = otherWIPN.asInstanceOf[PosIntPN]
			plusPIPN(posOther)
		}
	}
	override def timesWIPN (otherWIPN : WholeIntPN) : WholeIntPN = {
		if (otherWIPN.isZeroPN) otherWIPN // We don't have to construct a different zero
		else {
			val posOther = otherWIPN.asInstanceOf[PosIntPN]
			timesPIPN(posOther)
		}
	}
}
abstract class NegIntPN(myPosComplement : PosIntPN) extends IntegerPN with NegativePN {
	// This is a number which is known to be a negative integer, which is equal to  -1 * myPosComplement
	// Closed under plus, but inverted by times
	override def negatePN: PureNum = negateNIPN
	def negateNIPN : PosIntPN = myPosComplement
	def plusNIPN (otherNIPN : NegIntPN) : NegIntPN
	def timesNIPN(otherNIPN : NegIntPN) : PosIntPN
}


/*

private case class OnePN() extends PosIntPN {

}
// A natural number two or greater
private case class BigPosNat() extends PosIntPN {
}
trait PNUtils {
}
 */