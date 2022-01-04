package fun.gravax.axlp.core.num

private trait NumFactoryStuff

trait ProofPositive
trait PurePosIntFactory {
	// Thinking about wacky encodings...
	// A "nano" integer roughly fits in half a Java Byte  max 7
	// A "micro" integer roughly fits in a Java Byte  max +127
	// A "tiny" integer roughly fits in a Java Short  max + 32767
	// A "small" integer roughly fits in an Int  (32 bit signed)  	max 2,147,483,647 (2 * 10^9 billion)
	// A "medium" integer roughly fits in a Long (64 bit signed)	max 9,223,372,036,854,775,807 (9 * 10^18 quintillion)
	// A "large" integer roughly fits in the BigInt range, which is close to a billion decimal digits
	//  -2^Integer.MAX_VALUE (exclusive) to +2^Integer.MAX_VALUE (exclusive)
	// a "huge" integer is something bigger than large...

	// We have some different ways of ensuring that an input number meets expectations:
	// 		Demand a formal proof instance
	//		check and 'throw'
	//		'assert'
	//		'require'

	def fromPosScalaBigInt(posBI : BigInt, proofPos : ProofPositive) : PosIntPN = {
		??? // BigPosIntImpl(posBI)
	}
	def mkSmallPosIntPN(posSmall : Int) : PosIntPN = {
		if (posSmall >= 1) {
			val bigInt = BigInt(posSmall)
			val bippn = ??? // new PosIntBigImpl(bigInt)
			bippn
		} else throw new IllegalArgumentException(s"Expected positive integer, got $posSmall")
	}
	def getPos01 : PosIntPN = mkSmallPosIntPN(1)
	def getPos02 : PosIntPN = mkSmallPosIntPN(2)
	def getPos03 : PosIntPN = mkSmallPosIntPN(3)

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
	protected val myPosIntFactory : PurePosIntFactory
	def fromComplement(comp : PosIntPN) : NegIntPN = {
		// NegIntCompImpl(comp)
		???
	}
	def mkSmallNegIntPN(negSmall : Int) : NegIntPN = {
		require (negSmall < 0)
		val posCompl = negSmall * -1
		require (posCompl > 0)
		val posCompPN = myPosIntFactory.mkSmallPosIntPN(posCompl)
		fromComplement(posCompPN)
	}
	def getNeg01 : NegIntPN = mkSmallNegIntPN(-1)
	def getNeg02 : NegIntPN = mkSmallNegIntPN(-2)
	def fromNegScalaLong(sl : Long) : Option[NegIntPN] = ???
}
trait ProofZero
trait PureZeroFactory {
	val myZero = ZeroImpl()

	def getNum00 : ZeroPN = myZero
}

trait GenIntFactory extends PurePosIntFactory with PureNegIntFactory with PureZeroFactory

class NumFactoryImpl extends GenIntFactory {
	override protected val myPosIntFactory: PurePosIntFactory = this

	def mkReducedRatioOfPosInts(numer : PosIntPN, denom : PosIntPN) : PositivePN = {
		// But because numer is positive, we should know this ratio is positive
		val possiblyUnreducedRatio: PureNum = numer.divideByNonzeroPN(denom)
		// and we should know this reduced form is positive
		possiblyUnreducedRatio.reduceFractionPN.asInstanceOf[PositivePN]
	}

}
