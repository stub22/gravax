package fun.gravax.axlp.core.num

private trait NumImplStuff

abstract class PosIntBigImpl(positiveBigInt: BigInt) extends PureNumBaseImpl with PosIntPN {
	verifyInput
	private def verifyInput : Unit = {
		require (positiveBigInt.sign == 1)
	}

	override type ComplementType = NegIntPN
	override type ReciprocalType = PositivePN
	override type ReducedFracType = PosIntPN

	override def asScalaBigInt: Option[BigInt] = Some(positiveBigInt)
	/*

	 */
}
case class FullPIBI(posBI: BigInt) extends PosIntBigImpl(posBI) {
	override def timesPIPN(otherPIPN: PosIntPN): PosIntPN = ???

	override def plusIPN(otherIPN: IntegerPN): IntegerPN = ???

	override def timesIPN(otherIPN: IntegerPN): IntegerPN = ???

	override def plusNonnegPN(nngPN: NonnegPN): PositivePN = ???

	override protected def reciprocalPN: PositivePN = ???

	override def plusPIPN(otherPIPN: PosIntPN): PosIntPN = {
		val otherBI = otherPIPN.asScalaBigInt.get
		val sumBI = posBI + otherBI
		// TODO:  Allow the wrapper object to come from cache when this number is already in use.
		val sumPIPN = new FullPIBI(sumBI)
		sumPIPN
	}

	override def negatePN: NegIntPN = ???

	override def reduceFractionPN: PosIntPN = ???

	override def divideByNonzeroPN(otherPN: NonzeroPN): PureNum = ???

	override def plusPN(otherPN: PureNum): PureNum = ???

	override def timesPN(otherPN: PureNum): PureNum = ???

	override def isEqPN(otherPN: PureNum): Boolean = ???

	override def isGtPN(otherPN: PureNum): Boolean = ???
}

abstract  class NegIntCompImpl(complementPosInt : PosIntPN) extends NegNumBaseImpl with NegIntPN {
	override type ComplementType = PosIntPN
	override val myComplement = complementPosInt

	override type ReciprocalType = NegativePN
	override type ReducedFracType = NegIntPN

	private lazy val myBigInt_opt = complementPosInt.asScalaBigInt.map(_ * -1)
	override def asScalaBigInt: Option[BigInt] = myBigInt_opt

}

case class ZeroImpl() extends PureNumBaseImpl with ZeroPN {
	override def negatePN: ZeroPN = this

	override def plusIPN(otherIPN: IntegerPN): IntegerPN = otherIPN

	override def plusPN(otherPN: PureNum): PureNum = otherPN

	override def timesIPN(otherIPN: IntegerPN): IntegerPN = this

	override def timesPN(otherPN: PureNum): PureNum = this

	override def divideByNonzeroPN(otherPN: NonzeroPN): PureNum = this

	override def reduceFractionPN: ZeroPN = this

	override def safeReciprocalPN: Option[Nothing] = None

	override def isEqPN(otherPN: PureNum): Boolean = otherPN.reduceFractionPN.isZeroPN

	override def isGtPN(otherPN: PureNum): Boolean = otherPN.isNegativePN

	private lazy val myBigInt = BigInt(0)

	override def asScalaBigInt: Option[BigInt] = Some(myBigInt)


}
// TODO:  What if my reciprocal is optionally passed in, so we can hold on to mutual recips?
case class ReducedPosRatPairImpl(numer : PosIntPN, denom : PosIntPN) extends PureNumBaseImpl with PositivePN {
	override type ComplementType = NegativePN
	override type ReciprocalType = PositivePN
	override type ReducedFracType = PositivePN

	private def verifyReduced : Unit = {
		???
	}

	override protected def reciprocalPN: PositivePN = ReducedPosRatPairImpl(denom, numer)

	override def negatePN: NegativePN = ???

	override def reduceFractionPN: PositivePN = this

	override def isEqPN(otherPN: PureNum): Boolean = {
		???
	}

	override def isGtPN(otherPN: PureNum): Boolean = ???

	override def plusPN(otherPN: PureNum): PureNum = ???

	override def plusNonnegPN(nngPN: NonnegPN): PositivePN = ???

	override def timesPN(otherPN: PureNum): PureNum = ???

	override def divideByNonzeroPN(otherPN: NonzeroPN): PureNum = ???

	override val isIntegerPN: Boolean = {
		// Must reduce first
		???
	}

	override def asScalaBigDec: Option[BigDecimal] = ???


}
abstract class NegRatCompImpl(complementPosRat : PositivePN) extends NegNumBaseImpl  {

	override type ComplementType = PositivePN
	override type ReciprocalType = NegativePN
	override type ReducedFracType = NegativePN
	override val myComplement = complementPosRat

	override val isIntegerPN: Boolean = complementPosRat.isIntegerPN
}

/*

Scala BigInt offers
def gcd(that: BigInt): BigInt
Returns the greatest common divisor of abs(this) and abs(that)

Scala BigDecimal offers
def toString(): String
Returns the decimal String representation of this BigDecimal.

def isWhole(): Boolean
returns true if this number has no decimal component, false otherwise.

def  toBigIntExact(): Option[BigInt]
Converts this BigDecimal to a scala.BigInt if it can be done losslessly, returning Some(BigInt) or None.
 */