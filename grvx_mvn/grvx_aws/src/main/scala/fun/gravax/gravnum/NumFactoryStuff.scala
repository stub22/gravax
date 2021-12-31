package fun.gravax.gravnum

private trait NumFactoryStuff

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

