package fun.gravax.zdynamo

import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, Console, NonEmptyChunk, RIO, Scope, Task, TaskLayer, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, Random => ZRandom, dynamodb => ZDyn}

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

	lazy val myBinStore = new BinStoreApi {}

	lazy val myGenCtx = new GenCtx {
		override protected def getGenBD: GenBinData = new GenBinData {
			override val myTBI: ToBinItem = myBinStore.myTBI
		}
	}

	lazy val myConfGen = new ConfiguredGen(myGenCtx)

	private def mkProgram = {
		val justOneROFG = new PhonyFixedScenarioParams{
			override def getTgtTblNm: BinTag = myBinStore.binTblNm
		}
		val kcm = myConfGen.mkKeyedCmdMaker(justOneROFG)
		val bbg = myConfGen.mkBlockBaseGen(justOneROFG)

		val dumStore = new StoreDummyItems {}
		for {
			_ <- myBinStore.maybeCreateBinTable
			_ <- dumStore.putOneMessyItem
			_ <- dumStore.putOneDummyBinItem
			_ <- dumStore.readThatDummyBinYo
			secPK <- dumStore.putFeatherDBI
			rrslt <- myBinStore.readBinData(secPK)
			_ <- ZIO.log(s"Read binData at ${secPK} and got result: ${rrslt}")

			// _ <- dumpTagInfoStrm
			baseRslt <- genAndStoreBaseSqnc(justOneROFG, kcm, bbg) // : (myGenTN.BinTagNumBlock, Chunk[myGenBD.BinStoreRslt])
			// All of random data generation is now complete, so it is now OK if we do some operations multiple times.
			// All deterministic operations that start from baseRslt should give same answer.
			// If we didn't CHUNK the baseRslt (if we left it as a stream), then we would not have this repeatability.
			// This helps explains our two stage design : (oneBaseLevel, allVirtLevels)
			xtraBool <- extraChecksSqnc(baseRslt)
			virtRslt <- genAndStoreVirtSqnc(justOneROFG, kcm)(baseRslt)
			_ <- ZIO.log(s"Virt sqnc rslt: ${virtRslt}")
			_ <- myBinStore.maybeDeleteBinTable
		} yield ()
	}
	// All randomness of the scenario is encapsulated here in the base operation.
	// We store the full base result in RAM.
	// The full shape of the bin-tree numbering is also stored in RAM.
	def genAndStoreBaseSqnc(rofg : PhonyFixedScenarioParams, kcm : KeyedCmdMaker, bbg : BlockBaseGen): RIO[ZDynDBExec, BaseRsltPair] = {
		val massyMeatStrm = myConfGen.mkMassyMeatStrm(rofg)
		val keyedCmdMaker: KeyedCmdMaker = kcm //  myConfGen.ourKeyedCmdMaker
		val bsgnOp = bbg.genAndStoreBaseLevelOnly(keyedCmdMaker, massyMeatStrm)
		bsgnOp
	}
	// Deterministic virtual levels using the baseResults.
	// Numbering comes from baseResults
	def genAndStoreVirtSqnc(rofg : PhonyFixedScenarioParams, kcm : KeyedCmdMaker)(brPair : BaseRsltPair): ZIO[ZDynDBExec, Throwable, Chunk[BinStoreRslt]] = {
		val levRsltChnkStrm = myGenCtx.myBSTX.aggAndStoreVirtLevels(kcm)(brPair)
		val smootherOutStrm: ZStream[ZDynDBExec, Throwable, BinStoreRslt] = levRsltChnkStrm.flattenChunks
		val bigFlatOutputOp = smootherOutStrm.debug.runCollect
		bigFlatOutputOp
	}
	def extraChecksSqnc(brPair : BaseRsltPair) : UIO[Boolean] = {
		for {
			_ <- ZIO.succeed(myGenCtx.myGenBD.OLDE_computeParentMasses(brPair._2))
			combStat <- ZIO.succeed(myGenCtx.myBinSumCalc.combineWeightMeansAndVars(brPair._2))
			_ <- ZIO.log(s"Got combined stats: ${combStat}")
			// combineStatsPerParent : UIO[Chunk[(BinTagInfo, DBinWt, StatRow)]]
			parentStats <- myGenCtx.myBinSumCalc.combineStatsPerParent(brPair._2, brPair._1.getVirtLevelsChnk.last._2)
			_ <- ZIO.log(s"Got parent stats: ${parentStats}")
			pcomb <-   ZIO.succeed(myGenCtx.myBinSumCalc.combineVirtRsltsToWMV(parentStats))
			_ <- ZIO.log(s"Parents combined: ${pcomb}")
		} yield(true)
	}
	// standalone test runner for just the tagNum generator step
	def dumpTagInfoStrm: UIO[Chunk[((BinTagInfo, BinNumInfo), Long)]] = {
		val ps = myGenCtx.myGenTN.genTagInfoStrm(500, 7).zipWithIndex.take(300)
		val psOp = ps.debug.runCollect
		psOp
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