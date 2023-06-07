package test.gravax.trial

import fun.gravax.zaxlam.plain.{KnowsHappyVocab, PortfolioStatsLambda}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.{Map => SMap}

private trait DynLambSpecs

class FirstDynLambSpecs extends AnyFlatSpec with KnowsHappyVocab {
	"PortfolioStatsLambda" should "do local DB query" taggedAs(Tag_NoBatch) in {
		val pslam = new PortfolioStatsLambda()
		val inDat = SMap[String, AnyRef](MAPKEY_HAPPY_CMD -> HCMD_QUERY_BIN_DATA)
		val outDat = pslam.lambdaScala(inDat)
		println(s"Got outDat: ${outDat}")
	}

	"PortfolioStatsLambda" should "fail on missing cmdTxt" in {
		val pslam = new PortfolioStatsLambda()
		val inDat = SMap[String, AnyRef]()
		val outDat = pslam.lambdaScala(inDat)
		println(s"Got outDat: ${outDat}")
	}

}
