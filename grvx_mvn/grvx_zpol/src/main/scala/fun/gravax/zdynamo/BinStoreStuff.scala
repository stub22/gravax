package fun.gravax.zdynamo

import zio.dynamodb.{AttributeValue, DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, RIO, Scope, Task, TaskLayer, UIO, URIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, dynamodb => ZDyn}

import scala.collection.immutable.{Map => SMap}

private trait BinStoreStuff


trait BinStoreApi extends KnowsBinItem {
	val binTblNm = "distro-bin"
	val (flg_doCreate, flg_doDelete) = (false, false)

	val myFBI = new FromBinItem {}
	val myTBI = new ToBinItem {
		override protected val myFromItem: FromItem = myFBI
	}
	val myDIM = new DummyItemMaker {}

	def maybeCreateBinTable: RIO[ZDynDBExec, Unit] = if (flg_doCreate) {
		ZDynDBQry.createTable(binTblNm, binKeySchm, ZDyn.BillingMode.PayPerRequest)(
			scenAttr, sortKeyAttr).execute *> ZIO.log(s"Created table ${binTblNm}")
	} else ZIO.succeed()

	def maybeDeleteBinTable: RIO[ZDynDBExec, Unit] = if (flg_doDelete) {
		ZDynDBQry.deleteTable(binTblNm).execute *> ZIO.log(s"Deleted table ${binTblNm}")
	}  else ZIO.succeed()

	def putOneBigItem() : RIO[ZDynDBExec, Unit] = {
		val bigItem = myDIM.mkBigItem
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(binTblNm, bigItem)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[big] returned: ${opt_itm_out}"))
	}

	lazy val myDummyBinItem = myDIM.mkDummyBinItem
	def putOneDummyBinItem() : RIO[ZDynDBExec, Unit] = {
		putAndLog(binTblNm, myDummyBinItem)
	}

	def putSecondDBI : RIO[ZDynDBExec, PrimaryKey] = {
		// Can we easily check the size of the value-map (annRetMeans) in a filter condition?
		val (scen, binFlav) = ("featherScen", BFLV_ANN_RET_MEAN_VAR)
		val (timeObs, timePred, timeCalc) = ("20221209_21:30", "20231209_21:30", "20230105_14:18")
		val (binSeqNum, parentBinSeqNum)  = ("002", "001")
		val (binRelWeight, binAbsWeight, binMass) = (BigDecimal("0.32"), BigDecimal("0.0813"), BigDecimal("273"))

		val qqqEntry : BinTypes.StatEntry = ("QQQ", BigDecimal("0.0772"), BigDecimal("0.0430"))
		val spyEntry : BinTypes.StatEntry = ("SPY", BigDecimal("0.0613"), BigDecimal("0.0351"))
		val meatInfoMap : BinTypes.StatMap = SMap("QQQ" -> qqqEntry, "SPY" -> spyEntry)
		val meatInfo = BinMeatInfo(binFlav, meatInfoMap)

		val skelBinItem = myTBI.mkBinItemSkel(scen, timeObs, timePred, timeCalc)
		val fleshyBI = myTBI.fleshOutBinItem(skelBinItem, binSeqNum, parentBinSeqNum, binRelWeight, binAbsWeight, binMass)

		val meatyBI = myTBI.addMeatToBinItem(fleshyBI, meatInfo)
		val fullBI = myTBI.fillBinSortKey(meatyBI)
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
			val rm: Option[Either[DynamoDBError, Item]] = opt_itm_out.map(_.get[Item](FLDNM_BROKED_MEAT_MAP)) // "returns"))
			val opt_returns = rm.flatMap(_.toOption)
			// GOOG -> List(Chunk(Number(0.093),Number(0.018)))
			// dobleMap (via logB) is the form that actually works as of 23-05-02.  The others fail in decoding the List.
			val opt_googRet = opt_returns.map(_.get[Iterable[BigDecimal]]("GOOG"))
			val logA = ZIO.log(s"Item-get[dummy].logA broked-meat-map-item=${rm}, optRet=${opt_returns}, googRet=${opt_googRet} fullRecord=${opt_itm_out}")
			val dobleMap = opt_itm_out.map(itm => myFBI.fetchOrThrow[SMap[String,SMap[String,BigDecimal]]](itm, FLDNM_DOBLE_MAP))
			val logB = ZIO.log(s"Item-get[dummy].logB doble-map-item=${dobleMap}")


			val stringyMap_op: Task[Option[SMap[String, Iterable[String]]]] = ZIO.attempt(opt_itm_out.map(itm => myFBI.fetchOrThrow[SMap[String,Iterable[String]]](itm, FLDNM_STRINGY_MEAT_MAP)))
			val stringyEither: URIO[Any, Either[Throwable, Option[SMap[String, Iterable[String]]]]] = stringyMap_op.either
			val logC = stringyEither.flatMap(seith => ZIO.log(s"Item-get[dummy].logC stringy-either=${seith}"))
			val combo: UIO[Unit] = logA.zipRight(logB).zipRight(logC)
			combo
		})
		opLogged
	}
	// Getting BigDecimal SET
	// Error getting BigDecimal set value. Expected AttributeValue.Number but found List(Chunk(Number(0.093),Number(0.018)))))


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

