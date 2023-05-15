package fun.gravax.distrib.binstore

import fun.gravax.distrib.calc.BinSummaryCalc
import fun.gravax.distrib.gen.KnowsGenTypes
import fun.gravax.distrib.struct.{BinData, BinTimeInfo}
import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
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

	def maybeCreateBinTable: RIO[ZDynDBExec, Unit] = {
		println("println maybeCreateBinTable START")
		if (flg_doCreate) {
			ZDynDBQry.createTable(binTblNm, binKeySchm, ZDyn.BillingMode.PayPerRequest)(
				scenAttr, sortKeyAttr).execute *> ZIO.log(s"Created table ${binTblNm}")
		} else ZIO.succeed()
	}

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

// Fractional weight fields impose extra costs.  We need to know the total mass (of the distribution, == sum of all leaf bins)
// before we can complete writing any bins.  Otherwise we have to make a second pass after finding parent weights.
trait BinStoreCmdBuilder extends KnowsGenTypes {
	val myTBI : ToBinItem

	def makeBinStoreCmds(tblNm : String, scenID : String, timeInf : BinTimeInfo)(binSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow] = {
		val skelBintem: Item = myTBI.mkBinItemSkel(scenID, timeInf)
		val binLevelStoreTupStrm: UStream[BinStoreCmdRow] = binSpecStrm.map(bbSpec => {
			val (tagInfo, numInfo, massInfo, binMeat) = bbSpec
			val baseBinItem = myTBI.buildBinItem(skelBintem, tagInfo, massInfo, binMeat)
			val ourPK: PrimaryKey = myTBI.getFBI.getPKfromFullBinItem(baseBinItem)
			val putDynQry: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, baseBinItem)
			val putDynZIO: RIO[ZDynDBExec,Option[Item]] = putDynQry.execute
			(bbSpec, baseBinItem, ourPK, putDynZIO)
		})
		binLevelStoreTupStrm
	}

	// To process a Stream-of-ZIO we can use mapZIO, or more awkwardly runFoldZIO.
	def compileBinLevelStoreOp(storeCmdStrm : UStream[BinStoreCmdRow]) : RIO[ZDynDBExec, Chunk[BinStoreRslt]] = {
		val wovenCmdStream: ZStream[ZDynDBExec, Throwable, BinStoreRslt] = storeCmdStrm.mapZIO(cmdRow => {
			val (binSpec, binItem, binPK, binCmd) = cmdRow
			val enhCmd: RIO[ZDynDBExec, BinStoreRslt] = binCmd.map(rsltOptItm => (binSpec, binPK, rsltOptItm))
			enhCmd
		})
		val chnky: RIO[ZDynDBExec, Chunk[BinStoreRslt]] = wovenCmdStream.runCollect
		chnky
	}
}

trait BinStoreCmdXformer extends KnowsGenTypes {
	// Using java style here to pull in context data because it is context data to pull in.
	// protected def getGenBD : GenBinData
	protected def getBinSumCalc : BinSummaryCalc
	protected def getBSCB : BinStoreCmdBuilder

	private lazy val myBSCB = getBSCB
	// private lazy val myGenBD = getGenBD
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
			levStoreRslt <- myBSCB.compileBinLevelStoreOp(storeCmdStrm)
			_ <- ZIO.log(s"Got agg-parent levStoreRslt: ${levStoreRslt}")
		} yield(levStoreRslt)
	}

}

trait KeyedCmdMaker extends KnowsGenTypes {
	// All the info from outside of the bin needed to store the bin comes from here.
	def mkBaseLevCmds(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow]

	def mkAggLevCmds(aggRows : IndexedSeq[VirtRsltRow]) : UStream[BinStoreCmdRow]
}

