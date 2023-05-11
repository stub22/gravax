package fun.gravax.zdynamo

// import fun.gravax.zdynamo.RunZioDynamoTrial.{BaseRsltPair, BinStoreRslt, LevelNum, LevelTagNumChnk, aggAndStoreOneVirtLevel}
import zio.dynamodb.{AttributeValue, DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, RIO, Scope, Task, TaskLayer, UIO, URIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, dynamodb => ZDyn}

import scala.collection.immutable.{Map => SMap}

private trait BinStoreStuff

trait BinStoreApi extends KnowsBinItem { bsa =>
	val binTblNm = "distro-bin"
	val (flg_doCreate, flg_doDelete) = (false, false)

	val myFBI = new FromBinItem {}
	val myTBI = new ToBinItem {
		override protected val myFromItem = myFBI
	}
	val myDIM = new DummyItemMaker {}

	def maybeCreateBinTable: RIO[ZDynDBExec, Unit] = if (flg_doCreate) {
		ZDynDBQry.createTable(binTblNm, binKeySchm, ZDyn.BillingMode.PayPerRequest)(
			scenAttr, sortKeyAttr).execute *> ZIO.log(s"Created table ${binTblNm}")
	} else ZIO.succeed()

	def maybeDeleteBinTable: RIO[ZDynDBExec, Unit] = if (flg_doDelete) {
		ZDynDBQry.deleteTable(binTblNm).execute *> ZIO.log(s"Deleted table ${binTblNm}")
	}  else ZIO.succeed()


	def putAndLog(tblNm : String, itm : Item) : RIO[ZDynDBExec, Unit] = {
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, itm)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[${tblNm}] returned: ${opt_itm_out}"))
	}

	def readBinData(binPK : PrimaryKey) : RIO[ZDynDBExec, Option[BinData]] = {
		val op_itemFetch: RIO[ZDynDBExec,Option[Item]] = ZDynDBQry.getItem(binTblNm, binPK).execute
		val op_binDatFetch = op_itemFetch.map(opt_itm_out => {
			opt_itm_out.map(itm => {
				myFBI.extractBinData(itm)
			})
		})
		op_binDatFetch
	}
}

trait BinStoreTreeForFixedKey extends SpecialResultTypes {
	// Using java style here to pull in context data because it is context data to pull in.
	protected def getGenBD : GenBinData
	protected def getBinSumCalc : BinSummaryCalc
	protected def getKeyedCmdMaker : KeyedCmdMaker
	private lazy val myGenBD = getGenBD
	private lazy val myBinSumCalc = getBinSumCalc
	private lazy val myKeyedCmdMaker = getKeyedCmdMaker

	// From output stream of Chunks, can flattenChunks (strategic pivot) or just run as-is.
	def aggAndStoreVirtLevels(brPair : BaseRsltPair) : ZStream[ZDynDBExec, Throwable, Chunk[BinStoreRslt]] = { //  : RIO[ZDynDBExec, Chunk[BinStoreRslt]] = {
		val (tagNumBlock, baseRsltChnk) = brPair
		val vrtLvsUpFromBase: Chunk[(LevelNum, LevelTagNumChnk)] = tagNumBlock.getVirtLevelsChnk.reverse
		// We need to produce a sequence of effects, hence a (short-n-thick) stream will suffice.
		val vrtLvsStrm = ZStream.fromChunk(vrtLvsUpFromBase).debug
		// We need to carry the prev-result along, so we could use .runFoldZIO (building a List) or scanZIO
		val aggRsltStrm: ZStream[ZDynDBExec, Throwable, Chunk[BinStoreRslt]] = vrtLvsStrm.mapAccumZIO(baseRsltChnk)((prevRsltChnk, levPair) => {
			val (levNum, levTagChnk) = levPair
			// We yield nxtRsltChnk for both the stream-output and the next-state
			val storeOp: RIO[ZDynDBExec, Chunk[BinStoreRslt]] = aggAndStoreOneVirtLevel(levTagChnk, prevRsltChnk)
			val accumRsltOp = storeOp.map(nxtRsltChnk => (nxtRsltChnk, nxtRsltChnk))
			accumRsltOp
		})
		aggRsltStrm
	}
	def aggAndStoreOneVirtLevel(parentBlkNums : LevelTagNumChnk, childBlkRslt : Chunk[BinStoreRslt]) : RIO[ZDynDBExec, Chunk[BinStoreRslt]] = {
		// We can eagerly fetch these now, or allow the fetch to happen at effect time.

		for {
			aggParentStats <- myBinSumCalc.combineStatsPerParent(childBlkRslt, parentBlkNums)
			storeCmdStrm <- ZIO.succeed(myKeyedCmdMaker.mkAggLevCmds(aggParentStats))
			levStoreRslt <- myGenBD.compileBinLevelStoreOp(storeCmdStrm)
			_ <- ZIO.log(s"Got agg-parent levStoreRslt: ${levStoreRslt}")
		} yield(levStoreRslt)
	}

}
// These integer class-constructor parameters determine the shape of this generator setup.
abstract class GenRootXformer(rootTagNum : Int, rootKidsCnt : Int, baseBinLevel : Int) extends SpecialResultTypes {
	protected def getGenBD : GenBinData
	protected def getGenTN : GenTagNumData
	//protected def getKeyedCmdMaker : KeyedCmdMaker

	private lazy val myGenBD = getGenBD
	private lazy val myGenTN = getGenTN
	// private lazy val ourKeyedCmdMaker = getKeyedCmdMaker

	def genAndStoreBaseLevelOnly(keyedCmdMaker: KeyedCmdMaker, massyMeatStrm : UStream[(BinMassInfo, BinMeatInfo)]):
			RIO[ZDynDBExec, BaseRsltPair] = {
		val baseGenOp: RIO[ZDynDBExec, (myGenTN.BinTagNumBlock, Chunk[BinStoreRslt])] = for {
			bntgnmBlk <- myGenTN.genBinTagNumBlock(rootTagNum, rootKidsCnt, baseBinLevel)
			_ <- ZIO.log(s"genBaseSqnc .genBinTagNumBlock produced: ${bntgnmBlk.describe}")
			binSpecStrm <- ZIO.succeed(myGenBD.joinMassyMeatRows(bntgnmBlk.baseLevel, massyMeatStrm))
			binStoreCmdStrm <- ZIO.succeed(keyedCmdMaker.mkBaseLevCmds(binSpecStrm))
			levStoreRslt <- myGenBD.compileBinLevelStoreOp(binStoreCmdStrm)
			_ <- ZIO.log(s"Got levStoreRslt: ${levStoreRslt.toString().substring(0,200)} [TRUNCATED]")
		} yield(bntgnmBlk, levStoreRslt)
		baseGenOp
	}

}
trait KeyedCmdMaker extends KnowsGenTypes {
	// All the info from outside of the bin needed to store the bin comes from here.
	def mkBaseLevCmds(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow]

	def mkAggLevCmds(aggRows : IndexedSeq[VirtRsltRow]) : UStream[BinStoreCmdRow]
}

