package fun.gravax.distrib.binstore

import fun.gravax.distrib.struct.{BinMeatInfo, BinTypes}
import zio.dynamodb.{DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{RIO, Task, UIO, URIO, ZIO}
// import zio.{dynamodb => ZDyn}
import scala.collection.immutable.{Map => SMap}

private trait FakeDataStuff

trait StoreDummyItems extends BinStoreApi {
	// unit-testing methods that write+read some items with hardcoded data

	lazy val myDummyBinItem = myDIM.mkDummyBinItem
	def putOneDummyBinItem() : RIO[ZDynDBExec, Unit] = {
		putAndLog(binTblNm, myDummyBinItem)
	}

	def putOneMessyItem() : RIO[ZDynDBExec, Unit] = {
		println("println: putOneMessyItem START")
		val bigItem = myDIM.mkMessyItem
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(binTblNm, bigItem)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[big] returned: ${opt_itm_out}"))
	}

	def putFeatherDBI : RIO[ZDynDBExec, PrimaryKey] = {
		println("println: putFeatherDBI START")
		// Can we easily check the size of the value-map (annRetMeans) in a filter condition?
		val (scen, binFlav) = ("featherScen", BFLV_ANN_RET_MEAN_VAR)
		val (timeObs, timePred, timeCalc) = ("20221209_21:30", "20231209_21:30", "20230105_14:18")
		val (binSeqNum, parentBinSeqNum)  = ("002", "001")
		val (binMass, binRelWeight) = (BigDecimal("273"), BigDecimal("0.241")) //, BigDecimal("0.0813"), )

		val qqqEntry : BinTypes.StatEntry = ("QQQ", BigDecimal("0.0772"), BigDecimal("0.0430"))
		val spyEntry : BinTypes.StatEntry = ("SPY", BigDecimal("0.0613"), BigDecimal("0.0351"))
		val meatInfoMap : BinTypes.StatMap = SMap("QQQ" -> qqqEntry, "SPY" -> spyEntry)
		val meatInfo = BinMeatInfo(binFlav, meatInfoMap)

		val skelBinItem = myTBI.mkBinItemSkel(scen, timeObs, timePred, timeCalc)
		val fleshyBI = myTBI.fleshOutBinItem(skelBinItem, binSeqNum, parentBinSeqNum, binMass, binRelWeight) // , binAbsWeight, )

		val meatyBI = myTBI.addMeatToBinItem(fleshyBI, meatInfo)
		val fullBI = myTBI.fillBinSortKey(meatyBI)
		val ourPK: PrimaryKey = myFBI.getPKfromFullBinItem(fullBI)
		putAndLog(binTblNm, fullBI).map(_ => ourPK)
	}

	def readThatDummyBinYo() : RIO[ZDynDBExec, Unit] = {
		val dummyPK = myFBI.getPKfromFullBinItem(myDummyBinItem)
		val op: RIO[ZDynDBExec,Option[Item]] = ZDynDBQry.getItem(binTblNm, dummyPK).execute
		val opLogged = op.flatMap(opt_itm_out => {
			val rm: Option[Either[DynamoDBError, Item]] = opt_itm_out.map(_.get[Item](FLDNM_BROKED_MEAT_MAP)) // "returns"))
			val opt_returns = rm.flatMap(_.toOption)

			// dobleMap (via logB) is the form that actually works as of 23-05-02.  The others fail in decoding the List.
			// GOOG -> List(Chunk(Number(0.093),Number(0.018)))
			// Getting BigDecimal SET
			// Error getting BigDecimal set value. Expected AttributeValue.Number but found List(Chunk(Number(0.093),Number(0.018)))))
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

}