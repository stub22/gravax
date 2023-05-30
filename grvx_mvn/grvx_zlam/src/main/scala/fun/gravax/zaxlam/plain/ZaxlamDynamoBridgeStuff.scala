package fun.gravax.zaxlam.plain

import zio.{RIO, ZIO}
import fun.gravax.distrib.binstore.{BinStoreApi, DynLayerSetup}
import fun.gravax.distrib.gen.{DistribConsumer, KnowsGenTypes, PhonyFixedScenarioParams, UnsafeTaskRunner}
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}
import zio.{Random => ZRandom}
import zio.{Runtime => ZRuntime, Unsafe => ZUnsafe}

import java.util.{List => JList, Map => JMap}
import scala.collection.immutable.{Map => SMap}
import scala.jdk.CollectionConverters._

private trait ZaxlamDynamoBridgeStuff

class PortfolioStatsLambda extends HappyZaxlam with DistribConsumer with KnowsGenTypes {
	type TaskRslt = ZaxResult
	val fixedScenPrms = new PhonyFixedScenarioParams {
		override def getTgtTblNm: BinTag = myBinStore.myBinTblNm
	}
	override protected def makeBinStore: BinStoreApi = new BinStoreApi {}
	private val flgLocDb = true // true if local-only, false if remote AW$
	private val flgFromDocker = true
	private lazy val myDynLayerSetup = new DynLayerSetup(flgLocDb, flgFromDocker)

	// TODO:  Fetch some of our RDF metadata from S3, and use it to interpret the query.
	// TODO:  Try out some caching here in the lambda instance, for RDF + DynamoDB data.
	var myInstanceUsageCnt = 0;
	override protected def doQueryOp(inZSMap : ZaxSMap) : TaskRslt = {
		myInstanceUsageCnt += 1
		println(s"doQueryOp instanceUsageCnt=${myInstanceUsageCnt}")
		val qryTsk = mkQueryTask
		val wiredTask = myDynLayerSetup.wireDynamoTask[TaskRslt](qryTsk)
		val rslt = UnsafeTaskRunner.doRunTaskNow(wiredTask)
		rslt
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
			meatyPairChnk <- myBTEL.loadBinContentsEagerly(meatCache)(fixedScenPrms, maxLevels, maxBins)
			_ <- ZIO.log(s"mkDynProg_ReadSomeBins.loadBinContentsEagerly-meatyPairChnk size=${meatyPairChnk.size}, data=${meatyPairChnk}")
			cstts <- meatCache.cacheStats
			_ <- ZIO.log(s"CacheStats (hits,misses,size) = ${cstts}")
			rootBinNode <- myBTLL.loadBinTreeLazily(meatCache, meatKeyOrder)(fixedScenPrms, maxLevels, maxBins)
			_ <- ZIO.log(s"loadBinTreeLazily.rootBinNode = ${rootBinNode}")
			keysOfInterest <- myVDTH.chooseSomeKeys(ZRandom.RandomLive)(rootBinNode, covarKeyCnt)
			_ <- ZIO.log(s"chooseSomeKeys = ${keysOfInterest}")
			statMatrix <- myVDTH.computeCovars(rootBinNode)(keysOfInterest, covarDepth)
			_ <- ZIO.log(s"computeCovars.statMatrix = ${statMatrix}")
			zaxRslt <- ZIO.succeed(mkZaxResultForStatMatrix(statMatrix))
			_ <- ZIO.log(s"zaxRslt = ${zaxRslt}")
		} yield (zaxRslt) // .map to produce the output ZIO
		println("println END PortfolioStatsLambda.mkQueryTask")
		// myS4JLog.info("slf4j mkDynProg_ReadSomeBins END")
		queryTask
	}

	// type StatTriMatrix = IndexedSeq[(StatEntry, VwtCovRow)]
	// type StatEntry = (EntryKey, EntryMean, EntryVar)
	// type VwtCovRow = IndexedSeq[VwtCovTup]
	// type VwtCovTup = (EntryKey, EntryKey, VwtCov)
	// type VwtCov = BigDecimal
	// type EntryKey = String

	// type ZaxSMap = SMap[String, AnyRef]
	// type ZaxFlag = Boolean
	// type ZaxErr = (AnyRef,String)
	// type ZaxResult=Either[ZaxErr,ZaxSMap]
	// TODO:  Make number encoding more flexible.
	def mkZaxResultForStatMatrix(stm : StatTriMatrix) : TaskRslt = {
		val outSMap : SMap[EntryKey, AnyRef] = matrixToSMap(stm)
		// val outJMap = outSMap.asJava
		Right(outSMap)
	}
	val MAPKEY_SYM = "ENTRY_SYM"
	val MAPKEY_MEAN = "ENTRY_MEAN"
	val MAPKEY_VAR = "ENTRY_VAR"
	val MAPKEY_COV = "ENTRY_COV"

	type NumTxt = String

	type PerEntryField = String

	def matrixToSMap(stm : StatTriMatrix) : SMap[EntryKey, AnyRef]  = {
		stm.map(pair => {
			val ekey = pair._1._1
			val entrySMap = entryPairToSMap(pair)
			val entryJMap = entrySMap.asJava
			(ekey -> entryJMap)
		}).toMap
	}

	def entryPairToSMap(entryPair : (StatEntry, VwtCovRow)) : SMap[PerEntryField, AnyRef] = {
		val (statEnt, covRow) = entryPair
		val (ekey, emean, evar) = statEnt
		val emeanTxt : NumTxt = emean.toString()
		val evarTxt : NumTxt = evar.toString()
		val covRowSMap = covRowToSMap(covRow)
		// FIXME: Unjigger this Map-conversion mess!
		// Here the entryKey is the key of the *partner* entry that we share the covariance with.
		val covRowJMap: JMap[EntryKey, NumTxt] = covRowSMap.asJava
		val entryResultSMap = SMap[PerEntryField, AnyRef](MAPKEY_SYM -> ekey, MAPKEY_MEAN -> emeanTxt,
			MAPKEY_VAR -> evarTxt, MAPKEY_COV -> covRowJMap)
		entryResultSMap
	}

	def covRowToSMap(covRow: VwtCovRow) : SMap[EntryKey, NumTxt] = {
		val covMap: SMap[EntryKey, NumTxt] = covRow.map(covTup => {
			val (outerKey, innerKey, cov) = covTup
			val covTxt = cov.toString()
			(innerKey -> covTxt)
		}).toMap
		covMap
	}


}
