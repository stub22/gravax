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
	def isZeroPN : Boolean
	def isPositivePN : Boolean
	def isNegativePN : Boolean
	def isIntegerPN : Boolean
	def isPosIntPN : Boolean
	def isNegIntPN : Boolean
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
	override def isZeroPN: Boolean = true
	override def isPositivePN: Boolean = false
	override def isNegativePN: Boolean = false
}
trait NonzeroPN extends PureNum {
	override def isZeroPN: Boolean = false
}
trait PositivePN extends NonzeroPN {
	override def isPositivePN: Boolean = true
	override def isNegativePN: Boolean = false
}
trait NegativePN extends NonzeroPN {
	override def isPositivePN: Boolean = false
	override def isNegativePN: Boolean = true
}
// Sets that are closed over plus and times
trait IntegerPN extends PureNum {
	override def isIntegerPN: Boolean = true
	def plusIPN(otherIPN : IntegerPN) : IntegerPN
	def timesIPN(otherIPN : IntegerPN) : IntegerPN

}
trait WholeIntPN extends IntegerPN {
	def plusWIPN(otherWIPN : WholeIntPN) : WholeIntPN
	def timesWIPN (otherWIPN : WholeIntPN) : WholeIntPN
}
trait PosIntPN extends WholeIntPN with PositivePN {
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

private abstract class BigPosIntImpl(positiveBigInt: BigInt) extends BasePureNum with PosIntPN {
}
trait ProofPositive
trait PurePosIntFactory {
	def getPositiveOne : PosIntPN
	def fromPosScalaBigInt(posBI : BigInt, proofPos : ProofPositive) : PosIntPN = {
		??? // BigPosIntImpl(posBI)
	}
	def fromPosScalaLong(sl : Long) : Option[PosIntPN] = {
		if (sl > 0L) {
			val proof : ProofPositive = ???
			val pipn = fromPosScalaBigInt(BigInt(sl), proof)
			Some(pipn)
		} else None
	}
}
trait ProofNegative
trait PureNegIntFactory {
	def getNegativeOne : NegIntPN
	def fromNegScalaLong(sl : Long) : Option[NegIntPN] = ???
}
trait ProofZero
trait PureZeroFactory {
	def getZero : ZeroPN
}

trait GenIntFactory extends PurePosIntFactory with PureNegIntFactory with PureZeroFactory

/*

private case class OnePN() extends PosIntPN {

}
// A natural number two or greater
private case class BigPosNat() extends PosIntPN {
}
trait PNUtils {
}
 */