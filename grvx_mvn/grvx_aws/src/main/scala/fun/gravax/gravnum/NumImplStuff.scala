package fun.gravax.gravnum

private trait NumImplStuff

/*
class NegIntPureNumImpl(myPosComplement : PosIntPN) extends IntegerPN with NegativePN {
	// This is a number which is known to be a negative integer, which is equal to  -1 * myPosComplement
	// Closed under plus, but inverted by times
	override def negatePN: PureNum = negateNIPN
	def negateNIPN : PosIntPN = myPosComplement
	def plusNIPN (otherNIPN : NegIntPN) : NegIntPN
	def timesNIPN(otherNIPN : NegIntPN) : PosIntPN
}
 */

abstract class BigPosIntImpl(positiveBigInt: BigInt) extends PureNumBaseImpl with PosIntPN {
	override type ComplementType = NegIntPN
	override type ReciprocalType = PositivePN
	override type ReducedFracType = PosIntPN
}

abstract class NegIntImpl(complementPosInt : PosIntPN) extends NegNumBaseImpl with NegIntPN {
	override type ComplementType = PosIntPN
	override val myComplement = complementPosInt

	override type ReciprocalType = NegativePN
	override type ReducedFracType = NegIntPN
}

abstract class ZeroImpl extends PureNumBaseImpl with ZeroPN {
	override def plusIPN(otherIPN: IntegerPN): IntegerPN = otherIPN

	override def timesIPN(otherIPN: IntegerPN): IntegerPN = this

}