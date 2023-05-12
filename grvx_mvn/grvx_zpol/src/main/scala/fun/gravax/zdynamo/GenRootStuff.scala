package fun.gravax.zdynamo

import zio.stream.{UStream, ZStream}
import zio.{Chunk, NonEmptyChunk, RIO, UIO, ZIO, Random => ZRandom}

import java.math.{MathContext, RoundingMode}

private trait GenRootStuff

trait ScenarioParams extends KnowsGenTypes {
	def getTgtTblNm : String
	def getScenID : String
	def getTimeInf : BinTimeInfo
	def getBinFlav :  BinFlavor = BFLV_ANN_RET_MEAN_VAR
	val rootTagNum : Int = 1200
	val rootKidsCnt : Int  = 7
	val baseBinLevel : Int = 4
}

trait PhonyFixedScenarioParams extends ScenarioParams {
	override def getScenID = "gaussnoizBBB"
	override def getTimeInf = BinTimeInfo("NOPED", "TIMELESS", "NAKK")
}

trait GenCtx {
//	protected def getGenBD: GenBinData
	protected def getTBI : ToBinItem

	lazy val myOuterTBI = getTBI
	lazy val myGenBD = new GenBinData {	}
	lazy val myGenMM = new GenMeatAndMassData {}
	lazy val myGenTN = new GenTagNumData {}
	lazy val myBinSumCalc = new BinSummaryCalc {}
	lazy val myBDX = new BinDataXformer {}
	lazy val myBSCB = new BinStoreCmdBuilder {
		override val myTBI: ToBinItem = myOuterTBI
	}
	lazy val myBSTX = new BinStoreCmdXformer {
		// override protected def getGenBD: GenBinData = myGenBD

		override protected def getBinSumCalc: BinSummaryCalc = myBinSumCalc

		override protected def getBSCB: BinStoreCmdBuilder = myBSCB
	}
}

class ConfiguredGen(myGenCtx : GenCtx) extends KnowsGenTypes {

	def mkBlockBaseGen(scenParams : ScenarioParams) = new BlockBaseGen(scenParams.rootTagNum, scenParams.rootKidsCnt, scenParams.baseBinLevel) {
		override protected def getGenBD: GenBinData = myGenCtx.myGenBD

		override protected def getGenTN: GenTagNumData = myGenCtx.myGenTN

		override protected def getBSCB: BinStoreCmdBuilder = myGenCtx.myBSCB
	}

	def mkMassyMeatStrm(scenParams : ScenarioParams): UStream[(BinMassInfo, BinMeatInfo)] = {
		val precision = 8
		val mathCtx = new MathContext(precision, RoundingMode.HALF_UP)
		val massyMeatStrm = myGenCtx.myGenMM.mkMassyMeatStrm(ZRandom.RandomLive, mathCtx)(scenParams.getBinFlav)
		massyMeatStrm
	}

	def mkKeyedCmdMaker(scenParams : ScenarioParams) = new OurKeyedCmdMkr(scenParams.getTgtTblNm, scenParams.getScenID, scenParams.getTimeInf, scenParams.getBinFlav) {

		override protected def getBDX: BinDataXformer = myGenCtx.myBDX

		override protected def getBSCB: BinStoreCmdBuilder = myGenCtx.myBSCB
	}
}

class GenAndStoreModule(myBinStore : BinStoreApi, myGenCtx : GenCtx) extends KnowsGenTypes {
	import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}

	lazy val myConfGen = new ConfiguredGen(myGenCtx)

	def mkGenAndStoreOp(scenParams : ScenarioParams) = {
		// So far we are only making one set of params.

		val kcm = myConfGen.mkKeyedCmdMaker(scenParams)
		val bbg = myConfGen.mkBlockBaseGen(scenParams)

		for {
			baseRslt <- genAndStoreBaseSqnc(scenParams, kcm, bbg) // : (myGenTN.BinTagNumBlock, Chunk[myGenBD.BinStoreRslt])
			// All of random data generation is now complete, so it is now OK if we do some operations multiple times.
			// All deterministic operations that start from baseRslt should give same answer.
			// If we didn't CHUNK the baseRslt (if we left it as a stream), then we would not have this repeatability.
			// This helps explains our two stage design : (oneBaseLevel, allVirtLevels)
			xtraBool <- extraChecksSqnc(baseRslt)
			virtRslt <- genAndStoreVirtSqnc(scenParams, kcm)(baseRslt)
			_ <- ZIO.log(s"Virt sqnc rslt: ${virtRslt}")
		} yield(baseRslt, virtRslt)
	}

	// All randomness of the scenario is encapsulated here in the base operation.
	// We store the full base result in RAM.
	// The full shape of the bin-tree numbering is also stored in RAM.
	def genAndStoreBaseSqnc(scenParams : ScenarioParams, kcm : KeyedCmdMaker, bbg : BlockBaseGen): RIO[ZDynDBExec, BaseRsltPair] = {
		val massyMeatStrm = myConfGen.mkMassyMeatStrm(scenParams)
		val keyedCmdMaker: KeyedCmdMaker = kcm //  myConfGen.ourKeyedCmdMaker
		val bsgnOp = bbg.genAndStoreBaseLevelOnly(keyedCmdMaker, massyMeatStrm)
		bsgnOp
	}
	// Deterministic virtual levels using the baseResults.
	// Numbering comes from baseResults
	def genAndStoreVirtSqnc(scenParams : ScenarioParams, kcm : KeyedCmdMaker)(brPair : BaseRsltPair): ZIO[ZDynDBExec, Throwable, Chunk[BinStoreRslt]] = {
		val levRsltChnkStrm = myGenCtx.myBSTX.aggAndStoreVirtLevels(kcm)(brPair)
		val smootherOutStrm: ZStream[ZDynDBExec, Throwable, BinStoreRslt] = levRsltChnkStrm.flattenChunks
		val bigFlatOutputOp = smootherOutStrm.debug.runCollect
		bigFlatOutputOp
	}
	def extraChecksSqnc(brPair : BaseRsltPair) : UIO[Boolean] = {
		for {
			_ <- ZIO.succeed(myGenCtx.myGenBD.OLDE_computeParentMasses(brPair._2))
			combStat <- ZIO.succeed(myGenCtx.myBinSumCalc.combineWeightMeansAndVars(brPair._2))
			_ <- ZIO.log(s"Got combined stats: ${combStat}")
			// combineStatsPerParent : UIO[Chunk[(BinTagInfo, DBinWt, StatRow)]]
			parentStats <- myGenCtx.myBinSumCalc.combineStatsPerParent(brPair._2, brPair._1.getVirtLevelsChnk.last._2)
			_ <- ZIO.log(s"Got parent stats: ${parentStats}")
			pcomb <-   ZIO.succeed(myGenCtx.myBinSumCalc.combineVirtRsltsToWMV(parentStats))
			_ <- ZIO.log(s"Parents combined: ${pcomb}")
		} yield(true)
	}
}

