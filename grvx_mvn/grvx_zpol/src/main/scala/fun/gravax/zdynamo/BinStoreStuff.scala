package fun.gravax.zdynamo

import zio.dynamodb.{ Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, RIO, ZIO, dynamodb => ZDyn}

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

trait BinStoreTreeForFixedKey extends KnowsGenTypes {
	// Using java style here to pull in context data because it is context data to pull in.
	protected def getGenBD : GenBinData
	protected def getBinSumCalc : BinSummaryCalc

	private lazy val myGenBD = getGenBD
	private lazy val myBinSumCalc = getBinSumCalc

	// From output stream of Chunks, can flattenChunks (strategic pivot) or just run as-is.
	def aggAndStoreVirtLevels(keyedCmdMkr : KeyedCmdMaker)(brPair : BaseRsltPair) : ZStream[ZDynDBExec, Throwable, Chunk[BinStoreRslt]] = { //  : RIO[ZDynDBExec, Chunk[BinStoreRslt]] = {
		val (tagNumBlock, baseRsltChnk) = brPair
		val vrtLvsUpFromBase: Chunk[(LevelNum, LevelTagNumChnk)] = tagNumBlock.getVirtLevelsChnk.reverse
		// We need to produce a sequence of effects, hence a (short-n-thick) stream will suffice.
		val vrtLvsStrm = ZStream.fromChunk(vrtLvsUpFromBase).debug
		// We need to carry the prev-result along, so we could use .runFoldZIO (building a List) or scanZIO
		val aggRsltStrm: ZStream[ZDynDBExec, Throwable, Chunk[BinStoreRslt]] = vrtLvsStrm.mapAccumZIO(baseRsltChnk)((prevRsltChnk, levPair) => {
			val (levNum, levTagChnk) = levPair
			// We yield nxtRsltChnk for both the stream-output and the next-state
			val storeOp: RIO[ZDynDBExec, Chunk[BinStoreRslt]] = aggAndStoreOneVirtLevel(keyedCmdMkr)(levTagChnk, prevRsltChnk)
			val accumRsltOp = storeOp.map(nxtRsltChnk => (nxtRsltChnk, nxtRsltChnk))
			accumRsltOp
		})
		aggRsltStrm
	}
	def aggAndStoreOneVirtLevel(keyedCmdMkr : KeyedCmdMaker)(parentBlkNums : LevelTagNumChnk, childBlkRslt : Chunk[BinStoreRslt]) : RIO[ZDynDBExec, Chunk[BinStoreRslt]] = {
		// We can eagerly fetch these now, or allow the fetch to happen at effect time.

		for {
			aggParentStats <- myBinSumCalc.combineStatsPerParent(childBlkRslt, parentBlkNums)
			storeCmdStrm <- ZIO.succeed(keyedCmdMkr.mkAggLevCmds(aggParentStats))
			levStoreRslt <- myGenBD.compileBinLevelStoreOp(storeCmdStrm)
			_ <- ZIO.log(s"Got agg-parent levStoreRslt: ${levStoreRslt}")
		} yield(levStoreRslt)
	}

}
trait KeyedCmdMaker extends KnowsGenTypes {
	// All the info from outside of the bin needed to store the bin comes from here.
	def mkBaseLevCmds(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow]

	def mkAggLevCmds(aggRows : IndexedSeq[VirtRsltRow]) : UStream[BinStoreCmdRow]
}

