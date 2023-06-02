package fun.gravax.zaxlam.plain

import zio.{RIO, Task, ZIO, Random => ZRandom}
import fun.gravax.distrib.binstore.{BinStoreApi, DynLayerSetup}
import fun.gravax.distrib.gen.{DistribConsumer, KnowsGenTypes, PhonyFixedScenarioParams}
import fun.gravax.zaxlam.srvlss.UnsafeTaskRunner
import fun.gravax.zaxlam.xform.PortStatXforms
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}

import scala.collection.immutable.{Map => SMap}


private trait ZaxlamDynamoBridgeStuff

// TODO:  Fetch some of our RDF metadata from S3, and use it to interpret the query.
// TODO:  Try out some caching here in the lambda instance, for RDF + DynamoDB data.
// TODO:  Try capturing ZIO.time info
// TODO:  Try out ZIO parallel streams
// TODO:  Verify relationships among the potential loggers in each runtime env:  mySlf4j, ZIO.log, LambdaLog, println
// TODO:  Instantiate and run this Lambda from a unit test
class PortfolioStatsLambda extends HappyZaxlam  with DistribConsumer with KnowsGenTypes {
	type TaskRslt = ZaxResult
	val fixedScenPrms = new PhonyFixedScenarioParams {
		override def getTgtTblNm: BinTag = myBinStore.myBinTblNm
	}
	override protected def makeBinStore: BinStoreApi = new BinStoreApi {}
	private val flgLocDb = true // true if local-only, false if remote AW$
	private val flgFromDocker = true
	private lazy val myDynLayerSetup = new DynLayerSetup(flgLocDb, flgFromDocker)
	private lazy val myTaskRunner = new UnsafeTaskRunner {}
	private lazy val myStatsXformer = new PortStatXforms {}

	var myInstanceUsageCnt = 0;
	override protected def doQueryOp(inZSMap : ZaxSMap) : ZaxResult = {
		myInstanceUsageCnt += 1
		println(s"doQueryOp instanceUsageCnt=${myInstanceUsageCnt}")
		val qryTsk = mkQueryTask
		val wiredTask: Task[ZaxResult] = myDynLayerSetup.wireDynamoTask[ZaxResult](qryTsk)
		val rsltEither: Either[Throwable, ZaxResult] = myTaskRunner.doRunUnsafeTaskToEither(wiredTask)
		val zrslt : ZaxResult = rsltEither.fold(th => Left(th, th.toString), innerRslt => innerRslt)
		zrslt
		//	val rslt = myTaskRunner.doRunUnsafeTaskMightThrow(wiredTask)
		//	rslt
	}
	private def mkQueryTask: RIO[ZDynDBExec, TaskRslt] = {
		val (maxLevels, maxBins) = (3, 10)
		val (covarKeyCnt, covarDepth) = (6, 2)
		val meatKeyOrder : Ordering[EntryKey] = Ordering.String
		println("println START PortfolioStatsLambda.mkQueryTask")
		// myS4JLog.info("slf4j mkDynProg_ReadSomeBins START")
		val queryTask: ZIO[ZDynDBExec, Throwable, TaskRslt] = for {
			// First line of for comp is special because it eagerly creates our first Zio
			meatCache <- myMCM.makeMeatyItemCacheOp
// 			meatyPairChnk <- myBTEL.loadBinContentsEagerly(meatCache)(fixedScenPrms, maxLevels, maxBins)
//			_ <- ZIO.log(s"mkDynProg_ReadSomeBins.loadBinContentsEagerly-meatyPairChnk size=${meatyPairChnk.size}, data=${meatyPairChnk}")
			rootBinNode <- myBTLL.loadBinTreeLazily(meatCache, meatKeyOrder)(fixedScenPrms, maxLevels, maxBins)
			_ <- ZIO.log(s"loadBinTreeLazily.rootBinNode = ${rootBinNode}")
			keysOfInterest <- myVDTH.chooseSomeKeys(ZRandom.RandomLive)(rootBinNode, covarKeyCnt)
			_ <- ZIO.log(s"chooseSomeKeys = ${keysOfInterest}")
			statMatrix <- myVDTH.computeCovars(rootBinNode)(keysOfInterest, covarDepth)
			_ <- ZIO.log(s"computeCovars.statMatrix = ${statMatrix}")
			zaxRslt <- ZIO.succeed(myStatsXformer.mkZaxResultForStatMatrix(statMatrix))
			_ <- ZIO.log(s"zaxRslt = ${zaxRslt}")
			cstts <- meatCache.cacheStats
			_ <- ZIO.log(s"CacheStats (hits,misses,size) = ${cstts}")

		} yield (zaxRslt) // .map to produce the output ZIO
		println("println END PortfolioStatsLambda.mkQueryTask")
		// myS4JLog.info("slf4j mkDynProg_ReadSomeBins END")
		queryTask
	}

}
