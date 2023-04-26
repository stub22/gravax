package fun.gravax.zdynamo

import zio.dynamodb.{AttributeValue, DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, Console, RIO, Scope, Task, TaskLayer, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, dynamodb => ZDyn}
import zio.stream.ZStream

import java.time.{Instant => JInstant}
import scala.collection.immutable.{Map => SMap}

object RunZioDynamoTrial extends ZIOAppDefault {
	override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = mkTask

	def mkTask : Task[Unit] = {
		// ZDynDBExec is a trait defining this single method:
		//  def execute[A](atomicQuery : ZDynDBQry[_, A]) : zio.ZIO[scala.Any, scala.Throwable, A]
		/*
		TaskLayer[+ROut] is a type alias for ZLayer[Any, Throwable, ROut]
		type TaskLayer[+ROut] = ZLayer[Any, Throwable, ROut]
		 */
		val localDB_layer: TaskLayer[ZDynDBExec] = LocalDynamoDB.layer
		mkProgram.provide(localDB_layer)
	}

	private def mkProgram = {
		val bstore = new BinStoreApi {}
		for {
			_ <- bstore.maybeCreateBinTable
			_ <- bstore.putOneBigItem
			_ <- bstore.putOneDummyBinItem
			_ <- bstore.putSecondDBI
			_ <- bstore.readOneBin
			_ <- bstore.maybeDeleteBinTable
		} yield ()
	}
}

case class PredictionTimeInfo(obsTime : JInstant, calcTime : JInstant, endTime : JInstant)

trait BinModel {

	type QV // Quantitative value that acts like a Vector, allows computation of means.
	type ScenarioID = String

	type TimeInfo = PredictionTimeInfo


	case class BinRecord(scenario : ScenarioID, timeInf : TimeInfo, mean : QV, count_opt: Option[Int], weight_opt: Option[BigDecimal])

	def makeZDI(br : BinRecord) : Item
}

class MultiAssetBinModel extends BinModel {
	type AssetID = String

	type Amount = BigDecimal // M

	override type QV = SMap[String, Amount]

	override def makeZDI(br: BinRecord): Item = ???
}

trait BinStoreApi {
	val binTblNm = "distro-bin"
	val (flg_doCreate, flg_doDelete) = (false, false)

	// Scenario ID/URI is our partition key.  But we might want to add more data to this key to scale computation
	// within a scenario.
	val FLDNM_SCEN = "scenario"

	// Sort-key is made out of timestamps and sequence-numbers
	// calc_obs_fut_seqNum
	//

	// We want to use some combination of these time fields as dynamo-db sort key.
	// Dynamo wants that sort key to be a single attribute, so if there is going to be concatenation we have to manage it.
	val FLDNM_TIME_OBS = "time-obs"		// Time of the last observation the prediction is based on, e.g. last price quote.
	val FLDNM_TIME_CALC = "time-calc"	// Time the prediction was calculated.
	val FLDNM_TIME_PRED = "time-pred"		// Time of the predicted future observation, e.g. asset return time horizon.
	val FLDNM_BINSEQ = "bin-seq"
	val FLDNM_PARENT_BINSEQ = "parent-bin-seq"

	val FLDNM_BIN_REL_WEIGHT = "bin-rel-weight" // What is our mass's fraction of the parent bin mass?
	val FLDNM_BIN_ABS_WEIGHT = "bin-abs-weight" // What is our mass's fraction of the root bin mass? (optional)
	val FLDNM_BIN_MASS = "bin-obs-mass" // How many items have been observed by this bin?  (optional).  Should be the sum of child masses, if any.

	val FLDNM_BIN_FLAVOR = "bin-flavor"	// Enum telling us what kind of value-map this bin holds
	val BFLV_ANN_RET_MEAN = "ANN_RET_MEAN"
	// val BFLV_

	val FLDNM_ANN_RET_MEAN = "ann-ret-mean"

	val KEYNM_SORT_BIN = "sort-key"		// Some string-concat of the above times.  Could be scenario specific!
	val FLDSEQ_SORT_BIN = List(FLDNM_TIME_OBS, FLDNM_TIME_PRED, FLDNM_TIME_CALC, FLDNM_BINSEQ)
	val binKeySchm = ZDyn.KeySchema(FLDNM_SCEN, KEYNM_SORT_BIN)  // partition-key, sort-key

	// It seems we only need AttributeDefinitions for attributes that are used in keys ... or indices?
	val scenAttr = ZDyn.AttributeDefinition.attrDefnString(FLDNM_SCEN)
	// Examples seen so far use Strings for date values.
	val sortKeyAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_SORT_BIN)

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

	lazy val myDummyBinItem = mkBinItem
	def putOneDummyBinItem() : RIO[ZDynDBExec, Unit] = {
		putAndLog(binTblNm, myDummyBinItem)
	}
	def putSecondDBI = {
		val (scen, binFlav) = ("featherScen", BFLV_ANN_RET_MEAN)
		val (timeObs, timePred, timeCalc) = ("20221209_21:30", "20231209_21:30", "20230105_14:18")
		val (binSeqNum, parentBinSeqNum)  = ("002", "001")
		val (binRelWeight, binAbsWeight, binMass) = (BigDecimal("0.32"), BigDecimal("0.0813"), BigDecimal("273"))
		val annRetMeans = Map[String, BigDecimal](msftSym -> BigDecimal("0.0772"), googSym -> BigDecimal("0.0613"))
		val skelBinItem = mkBinItemSkel(scen, timeObs, timePred, timeCalc)
		val fleshyBI = fillBinItem(skelBinItem, binSeqNum, parentBinSeqNum, binRelWeight, binAbsWeight, binMass, binFlav, annRetMeans)
		val fullBI = fillBinSortKey(fleshyBI)
		putAndLog(binTblNm, fullBI)
	}

	def putAndLog(tblNm : String, itm : Item) : RIO[ZDynDBExec, Unit] = {
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, itm)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[${tblNm}] returned: ${opt_itm_out}"))
	}

	def readOneBin() : RIO[ZDynDBExec, Unit] = {
		val dummyPK = getPKfromFullBinItem(myDummyBinItem)
		val op: RIO[ZDynDBExec,Option[Item]] = ZDynDBQry.getItem(binTblNm, dummyPK).execute
		val opLogged = op.flatMap(opt_itm_out => {
			val rm: Option[Either[DynamoDBError, Item]] = opt_itm_out.map(_.get[Item](FLDNM_ANN_RET_MEAN)) // "returns"))
			val opt_returns = rm.flatMap(_.toOption)
			val opt_googRet: Option[Either[DynamoDBError, BigDecimal]] = opt_returns.map(_.get[BigDecimal](googSym))
			ZIO.log(s"s Item-get[weird] returnsItem=${rm}, googRet=${opt_googRet} fullRecord=${opt_itm_out}")
		})
		opLogged

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
		// Looking inside Workbench, we see that inside collection fields it dynamo often stores pairs of {type, txtVal},
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
		val fullBinItem = fillBinSortKey(partialBinItem)
		fullBinItem
	}
	def mkBinItemSkel(scen : String, timeObs : String, timePred : String, timeCalc : String) : Item = {
		Item(
			FLDNM_SCEN 		-> 	scen,
			FLDNM_TIME_OBS 	->	timeObs,
			FLDNM_TIME_PRED -> 	timePred,
			FLDNM_TIME_CALC -> 	timeCalc
		)
	}
	def fillBinItem(binWithSceneAndTimes : Item, binSeqNum : String, parentBinSeqNum : String,
					binRelWeight : BigDecimal, binAbsWeight : BigDecimal, binMass : BigDecimal,
					binFlavor : String, annRetMeans : Map[String, BigDecimal]) = {
		val addMap = Map[String, AttributeValue](
			FLDNM_BINSEQ -> AttributeValue(binSeqNum),
			FLDNM_PARENT_BINSEQ -> AttributeValue(parentBinSeqNum),
			FLDNM_BIN_REL_WEIGHT -> AttributeValue(binRelWeight),
			FLDNM_BIN_ABS_WEIGHT -> AttributeValue(binAbsWeight),
			FLDNM_BIN_MASS -> AttributeValue(binMass),
			FLDNM_BIN_FLAVOR -> AttributeValue(binFlavor),
			FLDNM_ANN_RET_MEAN -> AttributeValue(annRetMeans)
		)
		val comboMap = binWithSceneAndTimes.map ++ addMap
		val comboItem = Item(comboMap)
		comboItem
	}

	def getPKfromFullBinItem(fullBI : Item) = {
		val partitionKeyVal = fetchOrThrow[String](fullBI, FLDNM_SCEN)
		val sortKeyVal = fetchOrThrow[String](fullBI, KEYNM_SORT_BIN)
		PrimaryKey(FLDNM_SCEN	-> partitionKeyVal,	KEYNM_SORT_BIN -> sortKeyVal)
	}


	def fillBinSortKey(partBinItem : Item) : Item = fillSortKey(partBinItem, KEYNM_SORT_BIN, FLDSEQ_SORT_BIN, "#")

	// Will throw if any parts of the sort-key are not found in the partItm.
	def fillSortKey(partItm : Item, fn_sortKey : String, fns_sortKeyParts : Seq[String], separator : String) : Item = {
		// Assumes that sort-key part-values are all Strings.
		val pim: Map[String, AttributeValue] = partItm.map
		if (pim.contains(fn_sortKey)) partItm else {
			val sortKeyVals: Seq[String] = fns_sortKeyParts.map(skpn => fetchOrThrow[String](partItm, skpn))
			val compoundSortKey = sortKeyVals.mkString(separator)
			val attrVal_sortKey = AttributeValue(compoundSortKey)
			val fullPim = pim + (fn_sortKey -> attrVal_sortKey)
			val fullItem = Item(fullPim)
			fullItem
		}
	}

	private def fetchOrThrow[FVT](itm : Item, fldNm : String)(implicit ev : zio.dynamodb.FromAttributeValue[FVT]) : FVT = {
		val fv_eith: Either[DynamoDBError, FVT] = itm.get[FVT](fldNm)
		fv_eith match {
			case Left(ddbErr) => throw ddbErr
			case Right(fVal) => fVal
		}
	}

}
/*
              val query = DynamoDBQuery
                .queryAllItem(tableName)
                .whereKey($("id") === "id")
                .filter($("ttl").notExists)
              query.execute.flatMap(_.runDrain).exit.map { result =>
                assert(result)(succeeds(isUnit))
              }

              val query = DynamoDBQuery
                .querySomeItem(tableName, 1)
                .whereKey($("id") === "id")
                .filter($("ttl").notExists)
              query.execute.exit.map { result =>
                assert(result.isSuccess)(isTrue)
              }

https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html

For items with a given partition key value, DynamoDB stores these items close together, in sorted order by sort key value. In a Query operation, DynamoDB retrieves the items in sorted order, and then processes the items using KeyConditionExpression and any FilterExpression that might be present. Only then are the Query results sent back to the client.

A Query operation always returns a result set. If no matching items are found, the result set is empty.

Query results are always sorted by the sort key value. If the data type of the sort key is Number, the results are returned in numeric order. Otherwise, the results are returned in order of UTF-8 bytes. By default, the sort order is ascending. To reverse the order, set the ScanIndexForward parameter to false.

A single Query operation can retrieve a maximum of 1 MB of data. This limit applies before any FilterExpression or ProjectionExpression is applied to the results. If LastEvaluatedKey is present in the response and is non-null, you must paginate the result set (see Paginating table query results).

If you need to further refine the Query results, you can optionally provide a filter expression. A filter expression determines which items within the Query results should be returned to you. All of the other results are discarded.

A filter expression is applied after a Query finishes, but before the results are returned. Therefore, a Query consumes the same amount of read capacity, regardless of whether a filter expression is present.

A Query operation can retrieve a maximum of 1 MB of data. This limit applies before the filter expression is evaluated.

A filter expression cannot contain partition key or sort key attributes. You need to specify those attributes in the key condition expression, not the filter expression.

The syntax for a filter expression is similar to that of a key condition expression. Filter expressions can use the same comparators, functions, and logical operators as a key condition expression. In addition, filter expressions can use the not-equals operator (<>), the OR operator, the CONTAINS operator, the IN operator, the BEGINS_WITH operator, the BETWEEN operator, the EXISTS operator, and the SIZE operator. For more information, see Key condition expressions for query and Syntax for filter and condition expressions.
--------
In addition to the items that match your criteria, the Query response contains the following elements:

ScannedCount — The number of items that matched the key condition expression before a filter expression (if present) was applied.

Count — The number of items that remain after a filter expression (if present) was applied.

-----------------------
https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Query.html#API_Query_RequestParameters
---------------------

KeyConditionExpression
The condition that specifies the key values for items to be retrieved by the Query action.

The condition must perform an equality test on a single partition key value.

The condition can optionally perform one of several comparison tests on a single sort key value. This allows Query to retrieve one item with a given partition key value and sort key value, or several items that have the same partition key value but different sort key values.

The partition key equality test is required, and must be specified in the following format:

partitionKeyName = :partitionkeyval

If you also want to provide a condition for the sort key, it must be combined using AND with the condition for the sort key. Following is an example, using the = comparison operator for the sort key:

partitionKeyName = :partitionkeyval AND sortKeyName = :sortkeyval

Valid comparisons for the sort key condition are as follows:

sortKeyName = :sortkeyval - true if the sort key value is equal to :sortkeyval.

sortKeyName < :sortkeyval - true if the sort key value is less than :sortkeyval.

sortKeyName <= :sortkeyval - true if the sort key value is less than or equal to :sortkeyval.

sortKeyName > :sortkeyval - true if the sort key value is greater than :sortkeyval.

sortKeyName >= :sortkeyval - true if the sort key value is greater than or equal to :sortkeyval.

sortKeyName BETWEEN :sortkeyval1 AND :sortkeyval2 - true if the sort key value is greater than or equal to :sortkeyval1, and less than or equal to :sortkeyval2.

begins_with ( sortKeyName, :sortkeyval ) - true if the sort key value begins with a particular operand. (You cannot use this function with a sort key that is of type Number.) Note that the function name begins_with is case-sensitive.

Use the ExpressionAttributeValues parameter to replace tokens such as :partitionval and :sortval with actual values at runtime.

You can optionally use the ExpressionAttributeNames parameter to replace the names of the partition key and sort key with placeholder tokens. This option might be necessary if an attribute name conflicts with a DynamoDB reserved word.

------------

ProjectionExpression
A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.


 */