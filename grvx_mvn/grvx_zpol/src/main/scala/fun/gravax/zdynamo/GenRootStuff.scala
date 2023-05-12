package fun.gravax.zdynamo

import zio.dynamodb.{DynamoDBExecutor, Item, PrimaryKey}
import zio.stream.UStream
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
	protected def getGenBD: GenBinData

	lazy val myGenBD = getGenBD
	lazy val myGenMM = new GenMeatAndMassData {}
	lazy val myGenTN = new GenTagNumData {}
	lazy val myBinSumCalc = new BinSummaryCalc {}
	lazy val myBDX = new BinDataXformer {}
	lazy val myBSCB = new BinStoreCmdBuilder {
		override val myTBI: ToBinItem = myGenBD.myTBI
	}
	lazy val myBSTX = new BinStoreCmdXformer {
		// override protected def getGenBD: GenBinData = myGenBD

		override protected def getBinSumCalc: BinSummaryCalc = myBinSumCalc

		override protected def getBSCB: BinStoreCmdBuilder = myBSCB
	}

}


class ConfiguredGen(myGenCtx : GenCtx) extends KnowsGenTypes {

	// protected def getScenParams: ScenarioParams

	// lazy val myParams = getScenParams

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
/*
		override def mkBaseLevCmds(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow] = {
			myGenBD.makeBinStoreCmds(myParams.getTgtTblNm, myParams.getScenID, myParams.getTimeInf)(baseBinSpecStrm)
		}

		override def mkAggLevCmds(aggRows : IndexedSeq[myBinSumCalc.VirtRsltRow]) : UStream[BinStoreCmdRow] = {
			val binSpecStrm: UStream[BinSpec] = myBDX.aggStatsToBinSpecStrm(aggRows, myParams.getBinFlav)
			myGenBD.makeBinStoreCmds(myParams.getTgtTblNm, myParams.getScenID, myParams.getTimeInf)(binSpecStrm)
		}
*/

