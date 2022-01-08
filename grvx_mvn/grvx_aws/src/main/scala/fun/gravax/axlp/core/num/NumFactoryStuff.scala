package fun.gravax.axlp.core.num

private trait NumFactoryStuff

trait ProofPositive
trait PurePosIntFactory[PIPN <: PosIntPN] {
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

	def fromPosScalaBigInt(posBI : BigInt, proofPos : ProofPositive) : PIPN
	def mkSmallPosIntPN(posSmall : Int) : PIPN = {
		if (posSmall >= 1) {
			val smallishBigInt = BigInt(posSmall)
			val proofPos = new ProofPositive {}
			val bippn = fromPosScalaBigInt(smallishBigInt, proofPos)
			bippn
		} else throw new IllegalArgumentException(s"Expected positive integer, got $posSmall")
	}
	def getPos01 : PIPN = mkSmallPosIntPN(1)
	def getPos02 : PIPN = mkSmallPosIntPN(2)
	def getPos03 : PIPN = mkSmallPosIntPN(3)

	def fromPossiblyPositiveScalaLong(sl : Long) : Option[PIPN] = {
		if (sl > 0L) {
			val proofPos  = new ProofPositive {}
			val pipn = fromPosScalaBigInt(BigInt(sl), proofPos)
			Some(pipn)
		} else None
	}
}
trait ProofNegative
trait PureNegIntFactory[NIPN <: NegIntPN, CompPIPN <: PosIntPN] {
	protected val myPosIntFactory : PurePosIntFactory[CompPIPN]
	def fromComplement(comp : CompPIPN) : NIPN = {
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

trait GenIntFactory[PIPN <: PosIntPN, CompNIPN <: NegIntPN]
		extends PurePosIntFactory[PIPN]
		with PureNegIntFactory[CompNIPN, PIPN]
		with PureZeroFactory {
	override protected val myPosIntFactory: PurePosIntFactory[PIPN] = this
}

class SmallFreeIntFactory extends GenIntFactory[PosIntPN, NegIntPN] {
	override def fromPosScalaBigInt(posBI: BigInt, proofPos: ProofPositive): PosIntPN = {
		new FullPIBI(posBI)
	}

}
trait PracticeFreeNumFactory  {
// 	protected def mkIntFactory [PIPN <: PosIntPN, CompNIPN <: NegIntPN] : GenIntFactory[PIPN, CompNIPN] = ???
	// override protected val myPosIntFactory: PurePosIntFactory
	val myFreeIntFactory : SmallFreeIntFactory

	def mkReducedRatioOfPosInts(numer : PosIntPN, denom : PosIntPN) : PositivePN = {
		// But because numer is positive, we should know this ratio is positive
		val possiblyUnreducedRatio: PureNum = numer.divideByNonzeroPN(denom)
		// and we should know this reduced form is positive
		possiblyUnreducedRatio.reduceFractionPN.asInstanceOf[PositivePN]
	}

}

