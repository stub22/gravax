package fun.gravax.zdynamo

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

trait ConfiguredGen extends KnowsGenTypes {

	protected def getScenParams : ScenarioParams
	lazy val myParams = getScenParams

	protected def getGenBD : GenBinData
	private val myGenBD = getGenBD

	lazy val myGenMM = new GenMeatAndMassData {}
	lazy val myGenTN = new GenTagNumData {}
	lazy val myBinSumCalc = new BinSummaryCalc {}
	lazy val myBDX = new BinDataXformer {}
	lazy val myBSTX = new BinStoreTreeForFixedKey {
		override protected def getGenBD: GenBinData = myGenBD

		override protected def getBinSumCalc: BinSummaryCalc = myBinSumCalc
	}


	lazy val myBlockBaseGen = new BlockBaseGen(myParams.rootTagNum, myParams.rootKidsCnt, myParams.baseBinLevel) {
		override protected def getGenBD: GenBinData = myGenBD

		override protected def getGenTN: GenTagNumData = myGenTN
	}

	def mkMassyMeatStrm: UStream[(BinMassInfo, BinMeatInfo)] = {
		val precision = 8
		val mathCtx = new MathContext(precision, RoundingMode.HALF_UP)
		val massyMeatStrm = myGenMM.mkMassyMeatStrm(ZRandom.RandomLive, mathCtx)(myParams.getBinFlav)
		massyMeatStrm
	}

	lazy val ourKeyedCmdMaker = new KeyedCmdMaker {

		override def mkBaseLevCmds(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow] = {
			myGenBD.makeBinStoreCmds(myParams.getTgtTblNm, myParams.getScenID, myParams.getTimeInf)(baseBinSpecStrm)
		}

		override def mkAggLevCmds(aggRows : IndexedSeq[myBinSumCalc.VirtRsltRow]) : UStream[BinStoreCmdRow] = {
			val binSpecStrm: UStream[BinSpec] = myBDX.aggStatsToBinSpecStrm(aggRows, myParams.getBinFlav)
			myGenBD.makeBinStoreCmds(myParams.getTgtTblNm, myParams.getScenID, myParams.getTimeInf)(binSpecStrm)
		}

	}
}