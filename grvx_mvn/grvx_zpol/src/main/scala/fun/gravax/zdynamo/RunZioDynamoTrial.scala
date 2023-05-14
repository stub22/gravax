package fun.gravax.zdynamo

import fun.gravax.zdynamo.RunZioDynamoTrial.{BaseRsltPair, BinStoreRslt}
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, RIO, Scope, Task, TaskLayer, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

object RunZioDynamoTrial extends ZIOAppDefault with KnowsGenTypes {
	override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = mkTask

	/* type TaskLayer[+ROut] = ZLayer[Any, Throwable, ROut]
	// ZDynDBExec is a trait defining this single method:
	//  def execute[A](atomicQuery : ZDynDBQry[_, A]) : zio.ZIO[scala.Any, scala.Throwable, A]
 	*/

	def mkTask : Task[Unit] = {
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
	val fixedScenPrms = new PhonyFixedScenarioParams{
		override def getTgtTblNm: BinTag = myBinStore.binTblNm
	}
	val binWalker = new BinWalker {
		override protected def getBinStoreApi: BinStoreApi = myBinStore
	}

	private def mkProgram : ZIO[ZDynDBExec, Throwable, Unit] = {

		val dumStore = new StoreDummyItems {}
		println("println START mkProgram")
		val forBlock: ZIO[ZDynDBExec, Throwable, Unit] = for {
			// First line of for comp is special because it eagerly creates our first Zio
			_ <- myBinStore.maybeCreateBinTable	// FIRST line of for-comp code executes immediately to produce our FIRST Zio.
			_ <- dumStore.putOneMessyItem		// SECOND and further lines execute later in flatMap callbacks
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
			shamWowRslt <- binWalker.shamWow(fixedScenPrms, bdChnk)
			_ <- ZIO.log(s"shamWow result: ${shamWowRslt}")
			_ <- myBinStore.maybeDeleteBinTable
		} yield ("This result from RunZioDynamoTrial.mkProgram.forBlock may be ignored")	// .map to produce the output ZIO
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



/*
https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithItems.html

With GetItem, you must specify the entire primary key, not just part of it.
For example, if a table has a composite primary key (partition key and sort key), you must supply a value for the
partition key and a value for the sort key.
A GetItem request performs an eventually consistent read by default. You can use the ConsistentRead parameter to
request a strongly consistent read instead. (This consumes additional read capacity units, but it returns the most
up-to-date version of the item.)

GetItem returns all of the item's attributes. You can use a projection expression to return only some of the attributes.
For more information, see Projection expressions.

To return the number of read capacity units consumed by GetItem, set the ReturnConsumedCapacity parameter to TOTAL.

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