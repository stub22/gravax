package fun.gravax.distrib.gen

import fun.gravax.distrib.binstore.{BinStoreApi, BinWalker, DynLayerSetup, LocalDynamoDB, MeatCacheMaker, StoreDummyItems, ToBinItem}
import fun.gravax.distrib.struct.{BinNumInfo, BinTagInfo, BinTreeLoader}
import zio.cache.CacheStats
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}
import zio.{Chunk, RIO, Scope, Task, TaskLayer, UIO, URLayer, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

import java.net.URI

object RunDistribGenStoreLoadTrial extends ZIOAppDefault with KnowsGenTypes {
	// 4 booleans
	private lazy val myDynLayerSetup = new DynLayerSetup {
		override def getFlg_useLocalDB = true // true if local-only, false if remote AW$
	}
	protected def getFlg_doFullTableCycle : Boolean = false	// write data, optionally create/delete table
	lazy val myBinStore = new BinStoreApi {
		override val (flg_doCreate, flg_doDelete) = (false, false)
	}
	/*******************************************************************************/
	val myBinWalker = new BinWalker {
		override protected def getBinStoreApi: BinStoreApi = myBinStore
	}
	val myMCM = new MeatCacheMaker {
		override protected def getBinWalker: BinWalker = myBinWalker
	}
	val myBTL = new BinTreeLoader {
		override protected def getBinWalker: BinWalker = myBinWalker
	}

	lazy val myGenCtx = new GenCtx {
		override protected def getTBI: ToBinItem = myBinStore.myTBI
	}
	val myGenStoreModule = new GenAndStoreModule(myBinStore, myGenCtx)
	val fixedScenPrms = new PhonyFixedScenarioParams {
		override def getTgtTblNm: BinTag = myBinStore.binTblNm
	}
	/*******************************************************************************/

	override def run: Task[Unit] = {
		val program = if (getFlg_doFullTableCycle)	mkDynProg_WriteThenReadOneBin
		else mkDynProg_ReadSomeBins

		myDynLayerSetup.wireDynamoTask(program)
	}

	// Also may create and delete tables, mix margaritas, fix wagons
	private def mkDynProg_WriteThenReadOneBin: RIO[ZDynDBExec, Unit] = {
		val dumStore = new StoreDummyItems {}
		println("println START mkDynProg_WriteThenReadOneBin")
		val forBlock: ZIO[ZDynDBExec, Throwable, Unit] = for {
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
			meatCache <- myMCM.makeMeatyItemCacheOp
			//		shamWowRslt <- myBinWalker.shamWow(fixedScenPrms, bdChnk)
			//		_ <- ZIO.log(s"shamWow result: ${shamWowRslt}")
			_ <- myBinStore.maybeDeleteBinTable
		} yield ("This result from RunDistribGenStoreLoadTrial.mkDynProg_WriteThenReadOneBin.forBlock may be ignored") // .map to produce the output ZIO
		println("println END mkDynProg_WriteThenReadOneBin")
		forBlock.unit
	}
	private def mkDynProg_ReadSomeBins: RIO[ZDynDBExec, Unit] = {
		val (maxLevels, maxBins) = (2, 10)
		println("println START mkDynProg_ReadSomeBins")
		val forBlock: ZIO[ZDynDBExec, Throwable, Unit] = for {
			// TODO:  Go through BinTreeLoader and use meatCache
			// First line of for comp is special because it eagerly creates our first Zio
			meatCache <- myMCM.makeMeatyItemCacheOp
			meatyPairChnk <- myBTL.loadBinTreeEagerly(meatCache)(fixedScenPrms, maxLevels, maxBins)
			_ <- ZIO.log(s"mkDynProg_ReadSomeBins.loadBinTreeEagerly-meatyPairChnk size=${meatyPairChnk.size}, data=${meatyPairChnk}")
			cstts <- meatCache.cacheStats
			_ <- ZIO.log(s"CacheStats (hits,misses,size) = ${cstts}")
			rootBinNode <- myBTL.loadBinTreeLazily(meatCache)(fixedScenPrms, maxLevels, maxBins)
			_ <- ZIO.log(s"loadBinTreeLazily.rootBinNode = ${rootBinNode}")
		} yield ("This result from RunDistribGenStoreLoadTrial.mkDynProg_ReadSomeBins.forBlock may be ignored") // .map to produce the output ZIO
		println("println END mkDynProg_ReadSomeBins")
		forBlock.unit
	}
/*	def logCacheStats(meatCache: MeatyItemCache) = {
		val cchSttsOp = meatCache.cacheStats()
		val (hits, misses, size) = cacheStats.hi
	}
*/
	// standalone test runner for just the tagNum generator step
	def dumpTagInfoStrm: UIO[Chunk[((BinTagInfo, BinNumInfo), Long)]] = {
		val ps = myGenCtx.myGenTN.genTagInfoStrm(500, 7).zipWithIndex.take(300)
		val psOp = ps.debug.runCollect
		psOp
	}
}

// arn:aws:dynamodb:us-west-2:693649829226:table/distro-bin