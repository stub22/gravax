package fun.gravax.distrib.gen

import fun.gravax.distrib.binstore.{BinStoreApi, BinWalker, LocalDynamoDB, StoreDummyItems, ToBinItem}
import fun.gravax.distrib.struct.{BinNumInfo, BinTagInfo}
import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, Scope, Task, TaskLayer, UIO, ZIO, ZIOAppArgs, ZIOAppDefault}

object RunDistribGenStoreLoadTrial extends ZIOAppDefault with KnowsGenTypes {
	override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = mkTask

	/* type TaskLayer[+ROut] = ZLayer[Any, Throwable, ROut]
	// ZDynDBExec is a trait defining this single method:
	//  def execute[A](atomicQuery : ZDynDBQry[_, A]) : zio.ZIO[scala.Any, scala.Throwable, A]
 	*/

	def mkTask: Task[Unit] = {
		val localDB_layer: TaskLayer[ZDynDBExec] = LocalDynamoDB.layer
		mkProgram.provide(localDB_layer)
	}

	lazy val myBinStore = new BinStoreApi {
		override val (flg_doCreate, flg_doDelete) = (false, false)
	}
	lazy val myGenCtx = new GenCtx {
		override protected def getTBI: ToBinItem = myBinStore.myTBI
	}

	val myGenStoreModule = new GenAndStoreModule(myBinStore, myGenCtx)
	val fixedScenPrms = new PhonyFixedScenarioParams {
		override def getTgtTblNm: BinTag = myBinStore.binTblNm
	}
	val binWalker = new BinWalker {
		override protected def getBinStoreApi: BinStoreApi = myBinStore
	}

	private def mkProgram: ZIO[ZDynDBExec, Throwable, Unit] = {

		val dumStore = new StoreDummyItems {}
		println("println START mkProgram")
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
			qrslt <- binWalker.queryOp4BinScalars(fixedScenPrms)
			_ <- ZIO.log(s"queryOp4BinScalars result: ${qrslt}")
			bdChnk <- ZIO.succeed(binWalker.extractBinScalarsFromQRsltItems(qrslt._1))
			_ <- ZIO.log(s"extractBinScalarsFromQRsltItems result: ${bdChnk}")
			meatyBinItems <- binWalker.fetchMeatyBinItems(fixedScenPrms, bdChnk)
			_ <- ZIO.log(s"fetchMeatyBinItems result: ${meatyBinItems}")
			//		shamWowRslt <- binWalker.shamWow(fixedScenPrms, bdChnk)
			//		_ <- ZIO.log(s"shamWow result: ${shamWowRslt}")
			_ <- myBinStore.maybeDeleteBinTable
		} yield ("This result from RunDistribGenStoreLoadTrial.mkProgram.forBlock may be ignored") // .map to produce the output ZIO
		println("println END mkProgram")
		forBlock.unit
	}

	// standalone test runner for just the tagNum generator step
	def dumpTagInfoStrm: UIO[Chunk[((BinTagInfo, BinNumInfo), Long)]] = {
		val ps = myGenCtx.myGenTN.genTagInfoStrm(500, 7).zipWithIndex.take(300)
		val psOp = ps.debug.runCollect
		psOp
	}
}
