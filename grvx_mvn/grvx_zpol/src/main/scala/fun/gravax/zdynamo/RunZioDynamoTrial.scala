package fun.gravax.zdynamo

import zio.dynamodb.{DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
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
			_ <- bstore.putOneWeirdItem
			_ <- bstore.readOneWeird
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

	// We want to use some combination of these time fields as dynamo-db sort key.
	// Dynamo wants that sort key to be a single attribute, so if there is going to be concatenation we have to manage it.
	val FLDNM_TIME_OBS = "time-obs"		// Time of the last observation the prediction is based on, e.g. last price quote.
	val FLDNM_TIME_CALC = "time-calc"	// Time the prediction was calculated.
	val FLDNM_TIME_END = "time-end"		// Time of the predicted future observation, e.g. asset return time horizon.

	val FLDNM_SORT_KEY = "sort-key"		// Some string-concat of the above times.  Could be scenario specific!

	val binKeySchm = ZDyn.KeySchema(FLDNM_SCEN, FLDNM_SORT_KEY)  // partition-key, sort-key

	// It seems we only need AttributeDefinitions for attributes that are used in keys ... or indices?
	val scenAttr = ZDyn.AttributeDefinition.attrDefnString(FLDNM_SCEN)
	// Examples seen so far use Strings for date values.
	val sortKeyAttr = ZDyn.AttributeDefinition.attrDefnString(FLDNM_SORT_KEY)

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
	def putOneWeirdItem() : RIO[ZDynDBExec, Unit] = {
		val weirdItem = mkWeirdItem
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(binTblNm, weirdItem)
		val zpiex: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiex.flatMap(opt_itm_out => ZIO.log(s"s Item-put[weird] returned: ${opt_itm_out}"))
	}
	def readOneWeird() : RIO[ZDynDBExec, Unit] = {
		val pk = PrimaryKey(FLDNM_SCEN	-> scen01,	FLDNM_SORT_KEY -> sort_BB)

		val op: RIO[ZDynDBExec,Option[Item]] = ZDynDBQry.getItem(binTblNm, pk).execute
		val opLogged = op.flatMap(opt_itm_out => {
			val rm: Option[Either[DynamoDBError, Item]] = opt_itm_out.map(_.get[Item]("returns"))
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
			FLDNM_SORT_KEY -> sort_AA,
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
	def mkWeirdItem = {
		val weirdItem: Item = Item(
			FLDNM_SCEN	-> scen01,
			FLDNM_SORT_KEY -> sort_BB,
			"id"          -> 0,
			"bigNum" 		-> BigDecimal("12.72"),
			"bin"       -> Chunk.fromArray("abc".getBytes),
			"list"      -> List(1, 2, 3),
			"map"       -> SMap(
				"a" -> true,
				"d" -> false

			),
			"returns"	-> SMap(
				msftSym -> BigDecimal("0.117"),
				googSym -> BigDecimal("0.093")
			)
		)
		weirdItem
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


 */