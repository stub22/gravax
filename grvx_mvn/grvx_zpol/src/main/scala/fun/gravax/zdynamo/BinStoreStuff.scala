package fun.gravax.zdynamo

import zio.dynamodb.{AttributeValue, DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, RIO, Scope, Task, TaskLayer, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, dynamodb => ZDyn}
import scala.collection.immutable.{Map => SMap}

private trait BinStoreStuff


trait BinStoreApi extends KnowsBinItem {
	val binTblNm = "distro-bin"
	val (flg_doCreate, flg_doDelete) = (false, false)

	def maybeCreateBinTable: RIO[ZDynDBExec, Unit] = if (flg_doCreate) {
		ZDynDBQry.createTable(binTblNm, binKeySchm, ZDyn.BillingMode.PayPerRequest)(
			scenAttr, sortKeyAttr).execute *> ZIO.log(s"Created table ${binTblNm}")
	} else ZIO.succeed()

	def maybeDeleteBinTable: RIO[ZDynDBExec, Unit] = if (flg_doDelete) {
		ZDynDBQry.deleteTable(binTblNm).execute *> ZIO.log(s"Deleted table ${binTblNm}")
	}  else ZIO.succeed()

	def putOneBigItem() : RIO[ZDynDBExec, Unit] = {
		val bigItem = mkBigItem
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(binTblNm, bigItem)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[big] returned: ${opt_itm_out}"))
	}

	val myFBI = new FromBinItem {}
	val myTBI = new ToBinItem {
		override protected val myFromItem: FromItem = myFBI
	}

	lazy val myDummyBinItem = mkBinItem
	def putOneDummyBinItem() : RIO[ZDynDBExec, Unit] = {
		putAndLog(binTblNm, myDummyBinItem)
	}

	def putSecondDBI : RIO[ZDynDBExec, PrimaryKey] = {
		// Can we easily check the size of the value-map (annRetMeans) in a filter condition?
		val (scen, binFlav) = ("featherScen", BFLV_ANN_RET_MEAN)
		val (timeObs, timePred, timeCalc) = ("20221209_21:30", "20231209_21:30", "20230105_14:18")
		val (binSeqNum, parentBinSeqNum)  = ("002", "001")
		val (binRelWeight, binAbsWeight, binMass) = (BigDecimal("0.32"), BigDecimal("0.0813"), BigDecimal("273"))
		val annRetMeans = Map[String, BigDecimal](msftSym -> BigDecimal("0.0772"), googSym -> BigDecimal("0.0613"))
		val skelBinItem = myTBI.mkBinItemSkel(scen, timeObs, timePred, timeCalc)
		val fleshyBI =  myTBI.fleshOutBinItem(skelBinItem, binSeqNum, parentBinSeqNum, binRelWeight, binAbsWeight, binMass, binFlav, annRetMeans)
		val fullBI = myTBI.fillBinSortKey(fleshyBI)
		val ourPK: PrimaryKey = myFBI.getPKfromFullBinItem(fullBI)
		putAndLog(binTblNm, fullBI).map(_ => ourPK)
	}

	def putAndLog(tblNm : String, itm : Item) : RIO[ZDynDBExec, Unit] = {
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, itm)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[${tblNm}] returned: ${opt_itm_out}"))
	}

	def readThatDummyBinYo() : RIO[ZDynDBExec, Unit] = {
		val dummyPK = myFBI.getPKfromFullBinItem(myDummyBinItem)
		val op: RIO[ZDynDBExec,Option[Item]] = ZDynDBQry.getItem(binTblNm, dummyPK).execute
		val opLogged = op.flatMap(opt_itm_out => {
			val rm: Option[Either[DynamoDBError, Item]] = opt_itm_out.map(_.get[Item](FLDNM_ANN_RET_MEAN)) // "returns"))
			val opt_returns = rm.flatMap(_.toOption)
			val opt_googRet: Option[Either[DynamoDBError, BigDecimal]] = opt_returns.map(_.get[BigDecimal](googSym))
			ZIO.log(s"s Item-get[weird] returnsItem=${rm}, googRet=${opt_googRet} fullRecord=${opt_itm_out}")
		})
		opLogged
	}

	def mkBinData(itm : Item) : BinData = {
		???
	}
	def readBinData(binPK : PrimaryKey) : RIO[ZDynDBExec, Option[BinData]] = {
		val op_itemFetch: RIO[ZDynDBExec,Option[Item]] = ZDynDBQry.getItem(binTblNm, binPK).execute
		val op_binDatFetch = op_itemFetch.map(opt_itm_out => {
			opt_itm_out.map(itm => {
				mkBinData(itm)
			})
		})
		op_binDatFetch
	}

	val scen01 = "scen_01"
	val sort_AA = "20230423#20230421"
	val sort_BB = "20230423#20230421#bbb"
	val googSym = "GOOG"
	val msftSym = "MSFT"
	def mkBigItem = {
		val bigItem: Item = Item(
			FLDNM_SCEN	-> scen01,
			KEYNM_SORT_BIN -> sort_AA,
			"id"          -> 0,
			"bin"       -> Chunk.fromArray("abC".getBytes),
			"binSet"    -> Set(Chunk.fromArray("aBc".getBytes)),
			"boolean"   -> true,
			"list"      -> List(1, 2, 3, 7, 8),
			"map"       -> SMap(
				"a" -> true,
				"b" -> false,
				"c" -> true
			),
			"num"       -> 5,
			"numSet"    -> Set(4, 3, 2, 1),
			"null"      -> null,
			"string"    -> "string",
			"stringSet" -> Set("a", "b", "c")
		)
		bigItem
	}
	def mkBinItem = {
		// Looking via Workbench, we see that inside collection fields, dynamo often stores pairs of {type, txtVal},
		// where type is one of "N", "BOOL"...
		val partialBinItem : Item = Item(
			FLDNM_SCEN	-> scen01,
			FLDNM_TIME_OBS ->	"20221117_21:30",
			FLDNM_TIME_PRED -> "20231117_21:30",
			FLDNM_TIME_CALC -> "20221118_14:22",
			FLDNM_BINSEQ -> "001",
			FLDNM_PARENT_BINSEQ -> "-1",
			FLDNM_BIN_FLAVOR -> BFLV_ANN_RET_MEAN,  // Should this point directly to the value-map field?
			FLDNM_BIN_REL_WEIGHT -> BigDecimal("0.222"),
			FLDNM_BIN_ABS_WEIGHT -> BigDecimal("0.101"),
			FLDNM_BIN_MASS -> BigDecimal("255"),
			FLDNM_ANN_RET_MEAN	-> SMap(
				msftSym -> BigDecimal("0.117"),
				googSym -> BigDecimal("0.093")
			)
		)
		val fullBinItem = myTBI.fillBinSortKey(partialBinItem)
		fullBinItem
	}

}