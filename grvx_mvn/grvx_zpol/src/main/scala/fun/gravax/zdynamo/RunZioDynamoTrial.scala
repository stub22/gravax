package fun.gravax.zdynamo

import fun.gravax.zdynamo.RunZioDynamoTrial.gbd
import zio.dynamodb.{AttributeValue, DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, Console, RIO, Scope, Task, TaskLayer, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, Random => ZRandom, dynamodb => ZDyn}

import java.math.{MathContext, RoundingMode}
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

	lazy val bstore = new BinStoreApi {}
	private def mkProgram = {

		val dumStore = new StoreDummyItems {}
		for {
			_ <- bstore.maybeCreateBinTable
			_ <- dumStore.putOneMessyItem
			_ <- dumStore.putOneDummyBinItem
			_ <- dumStore.readThatDummyBinYo
			secPK <- dumStore.putFeatherDBI
			rrslt <- bstore.readBinData(secPK)
			_ <- ZIO.log(s"Read binData at ${secPK} and got result: ${rrslt}")

			// _ <- dumpTagInfoStrm
			// _ <- OLDE_simPyramid

			baseRslt <- genBaseSqnc

			_ <- bstore.maybeDeleteBinTable
		} yield ()
	}
	lazy val gbd = new GenBinData {
		override val myTBI: ToBinItem = bstore.myTBI
	}

	def genBaseSqnc: RIO[ZDynDBExec, (gbd.BinTagNumBlock, Chunk[gbd.BaseGenRsltRec])]   = {
		val rootTagNum = 1200
		val rootKidsCnt = 7
		val baseBinLevel = 4
		val massyMeatStrm = mkMassyMeatStrm
		val baseGenOp: RIO[ZDynDBExec, (gbd.BinTagNumBlock, Chunk[gbd.BaseGenRsltRec])] = for {
			bntgnmBlk <- gbd.genBinTagNumBlock(rootTagNum, rootKidsCnt, baseBinLevel)
			_ <- ZIO.log(s"genBaseSqnc .genBinTagNumBlock produced: ${bntgnmBlk.describe}")
			binSpecStrm <- ZIO.succeed(gbd.joinMassyMeatRows(bntgnmBlk.baseLevel, massyMeatStrm))
			binStoreCmdStrm <- ZIO.succeed(mkBaseLevCmds(binSpecStrm))
			levStoreRslt <- compileBaseLevelStoreOp(binStoreCmdStrm)
			_ <- ZIO.log(s"Got levStoreRslt: ${levStoreRslt}")
		} yield(bntgnmBlk, levStoreRslt)
		baseGenOp
	}


	def mkMassyMeatStrm = {
		val precision = 8
		val mathCtx = new MathContext(precision, RoundingMode.HALF_UP)
		val massyMeatStrm = gbd.mkMassyMeatStrm(ZRandom.RandomLive, mathCtx)
		massyMeatStrm
	}
	def mkBaseLevCmds(baseBinSpecStrm : UStream[gbd.BaseBinSpec]) : UStream[gbd.BaseBinStoreCmdRow] = {
		val scenID = "gaussnoizBBB"
		val timeInf = BinTimeInfo("NOPED", "TIMELESS", "NAKK")
		gbd.makeBaseBinStoreCmds(bstore.binTblNm, scenID, timeInf)(baseBinSpecStrm)
	}
	def compileBaseLevelStoreOp(storeCmdStrm : UStream[gbd.BaseBinStoreCmdRow]) : RIO[ZDynDBExec, Chunk[gbd.BaseGenRsltRec]] = {
		val wovenCmdStream: ZStream[ZDynDBExec, Throwable, gbd.BaseGenRsltRec] = storeCmdStrm.mapZIO(cmdRow => {
			val (binSpec, binItem, binPK, binCmd) = cmdRow
			val enhCmd: RIO[ZDynDBExec, gbd.BaseGenRsltRec] = binCmd.map(rsltOptItm => (binSpec, binPK, rsltOptItm))
			enhCmd
		})
		val chnky: RIO[ZDynDBExec, Chunk[gbd.BaseGenRsltRec]] = wovenCmdStream.runCollect
		chnky
	}

	def dumpTagInfoStrm  = {
		val ps = gbd.genTagInfoStrm(500, 7).zipWithIndex.take(300)
		val psOp = ps.debug.runCollect
		psOp
	}

	def OLDE_simPyramid : RIO[ZDynDBExec, Option[Item]] = {
		val massyMeatStrm = mkMassyMeatStrm
		val rootKidsCnt = 7
		val baseBinLevel = 4
		val scenID = "gaussnoizAAA"
		val timeInf = BinTimeInfo("NOPE", "TIMELESS", "NAK")
		val storCmdStream : UStream[gbd.BaseBinStoreCmdRow] = gbd.OLDE_genRandBinBaseLevel(bstore.binTblNm, scenID, timeInf)(massyMeatStrm, rootKidsCnt, baseBinLevel)
		val dumpOnlyZIO: UIO[Unit] = storCmdStream.foreach(cmdRow => ZIO.log(s"cmdRow: ${cmdRow}"))
		val cmdStream: UStream[RIO[ZDynDBExec, Option[Item]]] = storCmdStream.debug.map(cmdRow => cmdRow._4)
		// To process the Stream-of-ZIO we can use mapZIO, or runFoldZIO.  Latter q&d works but result is useless.
		// Could gather result values into a list by flatmapping the cmd
		// This experiment worked, but ZStream.mapZIO fits better, gets us back to a regular stream.
		val emptyInit : Option[Item] = None
		val oneBigStorageOp: RIO[ZDynDBExec, Option[Item]] = cmdStream.runFoldZIO(emptyInit)((optItm, cmd) => cmd)
		val zeroBD = BigDecimal("0.0")
		val emptyMassTally = SMap[String, BigDecimal]()
		// val recs = enhCmdStrm.runFold(emptyMassTally)((massTally, rsltRow) =>
		oneBigStorageOp
		// dumpOnlyZIO
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