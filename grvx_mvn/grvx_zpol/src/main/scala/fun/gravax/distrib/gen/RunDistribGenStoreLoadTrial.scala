package fun.gravax.distrib.gen

import fun.gravax.distrib.binstore.{BinStoreApi, BinWalker, DynLayerSetup, LocalDynamoDB, MeatCacheMaker, StoreDummyItems, ToBinItem}
import fun.gravax.distrib.gen.RunDistribGenStoreLoadTrial.myTaskMaker
import fun.gravax.distrib.struct.{BinDataEagerLoader, BinNumInfo, BinTagInfo, BinTreeLazyLoader, BinTreeLoader, VecDistTestHelper}
import org.slf4j.LoggerFactory
import zio.cache.CacheStats
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}
import zio.{Chunk, RIO, Scope, Task, TaskLayer, UIO, URLayer, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, Random => ZRandom}
import zio.{Runtime => ZRuntime, Unsafe => ZUnsafe}

import java.net.URI

object RunDistribGenStoreLoadTrial extends ZIOAppDefault {

	lazy val myTaskMaker = new DistribGenStoreLoadTrial()

	override def run: Task[Unit] = {
		val task = myTaskMaker.mkQuietDbTask
		task
	}
}

object RunDistribGenStoreLoadTrialFromMain {
	// import zio._
	val locDbFlgOpt = Some(false)
	lazy val myTaskMaker = new DistribGenStoreLoadTrial(locDbFlgOpt)
	def main(args: Array[String]): Unit = {
		println("RunDistribGenStoreLoadTrialFromMain.println says hello!")
		val task = myTaskMaker.mkQuietDbTask
		UnsafeTaskRunner.doRunNow(task)
		println("RunDistribGenStoreLoadTrialFromMain.println says byebye!")
	}
}

object UnsafeTaskRunner {
	def doRunNow(task : Task[Unit]) : Unit = {
		doRunTaskNow(task)
/*		println(s"======================= UnsafeTaskRunner START, inputTask=${task}")
		val zioRuntime = ZRuntime.default
		println(s"UnsafeTaskRunner zioRuntime=${zioRuntime}")
		ZUnsafe.unsafe { implicit unsafeThingy =>
			zioRuntime.unsafe.run(task).getOrThrowFiberFailure()
		}
		println("======================== UnsafeTaskRunner END")
 */
	}
	def doRunTaskNow[Rslt](task : Task[Rslt]) : Rslt = {
		println(s"======================= UnsafeTaskRunner START, inputTask=${task}")
		val zioRuntime = ZRuntime.default
		println(s"UnsafeTaskRunner zioRuntime=${zioRuntime}")
		val r : Rslt = ZUnsafe.unsafe { implicit unsafeThingy =>
			zioRuntime.unsafe.run(task).getOrThrowFiberFailure()
		}
		println("======================== UnsafeTaskRunner END")
		r
	}
}

trait DistribConsumer {
	protected  lazy val myBinStore = makeBinStore
	protected def makeBinStore : BinStoreApi

	protected lazy val myBinWalker = new BinWalker {
		override protected def getBinStoreApi: BinStoreApi = myBinStore
	}
	protected lazy val myMCM = new MeatCacheMaker {
		override protected def getBinWalker: BinWalker = myBinWalker
	}
	protected lazy val myBTEL = new BinDataEagerLoader {
		override protected def getBinWalker: BinWalker = myBinWalker
	}
	protected lazy val myBTLL = new BinTreeLazyLoader {
		override protected def getBinWalker: BinWalker = myBinWalker
	}
	protected lazy val myVDTH = new VecDistTestHelper{}

}

class DistribGenStoreLoadTrial(flgLocDbOpt : Option[Boolean] = None) extends KnowsGenTypes with DistribConsumer {
	// 4 booleans
	private val flgLocDb = flgLocDbOpt.getOrElse(true) // true if local-only, false if remote AW$
	private val flgFromDocker = true
	private lazy val myDynLayerSetup = new DynLayerSetup(flgLocDb, flgFromDocker)

	protected def getFlg_doFullTableCycle : Boolean = false	// write data, optionally create/delete table
	override protected def makeBinStore = new BinStoreApi {
		override val (flg_createTbl, flg_deleteTbl) = (false, false)
	}
	/*******************************************************************************/

	lazy val myGenCtx = new GenCtx {
		override protected def getTBI: ToBinItem = myBinStore.myTBI
	}
	val myGenStoreModule = new GenAndStoreModule(myBinStore, myGenCtx)
	val fixedScenPrms = new PhonyFixedScenarioParams {
		override def getTgtTblNm: BinTag = myBinStore.myBinTblNm
	}

	private val myS4JLog = LoggerFactory.getLogger(this.getClass)
	/*******************************************************************************/
	// Quiet because it doesn't return anything
	def mkQuietDbTask: Task[Unit] = {
		myS4JLog.info("mkQuietDbTask START")
		val program: RIO[ZDynDBExec, Unit] = if (getFlg_doFullTableCycle)	mkDynProg_WriteThenReadOneDistrib
		else mkDynProg_ReadSomeBins

		val wiredTask = myDynLayerSetup.wireDynamoTask[Unit](program)
		myS4JLog.info("mkQuietDbTask END")
		wiredTask
	}

	// Also may create and delete tables, mix margaritas, fix wagons
	private def mkDynProg_WriteThenReadOneDistrib: RIO[ZDynDBExec, Unit] = {
		val dumStore = new StoreDummyItems {}
		println("println START mkDynProg_WriteThenReadOneBin")
		val forBlock: ZIO[ZDynDBExec, Throwable, String] = for {
			// First line of for comp is special because it eagerly creates our first Zio
			_ <- myBinStore.maybeCreateBinTable // FIRST line of for-comp code executes immediately to produce our FIRST Zio.
			_ <- dumStore.putOneMessyItem // SECOND and further lines execute later in flatMap callbacks
			_ <- dumStore.putOneDummyBinItem
			_ <- dumStore.readThatDummyBinYo
			secPK <- dumStore.putFeatherDBI
			rrslt <- myBinStore.readBinData(secPK)
			_ <- ZIO.log(s"Read binData at ${secPK} and got result: ${rrslt}")
			// _ <- dumpTagInfoStrm
			rsltTup <- myGenStoreModule.mkGenAndStoreOp(fixedScenPrms)
			qrslt <- myBinWalker.queryOp4BinScalars(fixedScenPrms)
			_ <- ZIO.log(s"queryOp4BinScalars result: ${qrslt}")
			bdChnk <- ZIO.succeed(myBinWalker.extractBinScalarsFromQRsltItems(qrslt._1))
			_ <- ZIO.log(s"extractBinScalarsFromQRsltItems result: ${bdChnk}")
			meatyBinItems <- myBinWalker.fetchMeatyBinItems(fixedScenPrms, bdChnk)
			_ <- ZIO.log(s"fetchMeatyBinItems result: ${meatyBinItems}")
			_ <- myBinStore.maybeDeleteBinTable
		} yield ("This result from RunDistribGenStoreLoadTrial.mkDynProg_WriteThenReadOneBin.forBlock may be ignored") // .map to produce the output ZIO
		println("println END mkDynProg_WriteThenReadOneBin")
		forBlock.unit
	}
	private def mkDynProg_ReadSomeBins: RIO[ZDynDBExec, Unit] = {
		val (maxLevels, maxBins) = (3, 10)
		val (covarKeyCnt, covarDepth) = (6, 2)
		val meatKeyOrder : Ordering[EntryKey] = Ordering.String
		println("println START mkDynProg_ReadSomeBins")
		myS4JLog.info("slf4j mkDynProg_ReadSomeBins START")
		val forBlock: ZIO[ZDynDBExec, Throwable, String] = for {
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
		} yield ("This result from RunDistribGenStoreLoadTrial.mkDynProg_ReadSomeBins.forBlock may be ignored") // .map to produce the output ZIO
		println("println END mkDynProg_ReadSomeBins")
		myS4JLog.info("slf4j mkDynProg_ReadSomeBins END")
		forBlock.unit
	}

	// standalone test runner for just the tagNum generator step
	def dumpTagInfoStrm: UIO[Chunk[((BinTagInfo, BinNumInfo), Long)]] = {
		val ps = myGenCtx.myGenTN.genTagInfoStrm(fixedScenPrms.getBinFlav)(500, 7).zipWithIndex.take(300)
		val psOp = ps.debug.runCollect
		psOp
	}
}
