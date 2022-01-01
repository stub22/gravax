package fun.gravax.gravnum

private trait NumImplStuff

abstract class BigPosIntImpl(positiveBigInt: BigInt) extends BasePureNum with PosIntPN {
}
abstract class FakeBPI(positiveBigInt: BigInt) extends BigPosIntImpl(positiveBigInt) {
	override def plusPIPN(otherPIPN: PosIntPN): PosIntPN = ???

	override def timesPIPN(otherPIPN: PosIntPN): PosIntPN = ???

	override def plusIPN(otherIPN: IntegerPN): IntegerPN = ???

	override def timesIPN(otherIPN: IntegerPN): IntegerPN = ???

	override def plusPN(otherPN: PureNum): PureNum = ???

	override def timesPN(otherPN: PureNum): PureNum = ???

	override def isEqPN(otherPN: PureNum): Boolean = ???

	override def isGtPN(otherPN: PureNum): Boolean = ???

	override def isGteqPN(otherPN: PureNum): Boolean = ???

	override def safeReciprocalPN: Option[PureNum] = ???

	override def negatePN: PureNum = ???

	override def reduceFractionPN: PureNum = ???

}
abstract class FakeNPI(complementPIPN : PosIntPN) extends NegIntPN(complementPIPN) {
	override def plusNIPN(otherNIPN: NegIntPN): NegIntPN = ???

	override def timesNIPN(otherNIPN: NegIntPN): PosIntPN = ???

	override def plusIPN(otherIPN: IntegerPN): IntegerPN = ???

	override def timesIPN(otherIPN: IntegerPN): IntegerPN = ???

	override def minusPN(otherPN: PureNum): PureNum = ???

	override def safelyDivideByPN(otherPN: PureNum): Option[PureNum] = ???

	override def plusPN(otherPN: PureNum): PureNum = ???

	override def timesPN(otherPN: PureNum): PureNum = ???

	override def isEqPN(otherPN: PureNum): Boolean = ???

	override def isGtPN(otherPN: PureNum): Boolean = ???

	override def isGteqPN(otherPN: PureNum): Boolean = ???

	override def isLtPN(otherPN: PureNum): Boolean = ???

	override def isLteqPN(otherPN: PureNum): Boolean = ???

	override def safeReciprocalPN: Option[PureNum] = ???

	override def reduceFractionPN: PureNum = ???

}
