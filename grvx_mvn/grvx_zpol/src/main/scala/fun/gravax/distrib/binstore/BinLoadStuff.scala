package fun.gravax.distrib.binstore

import fun.gravax.distrib.gen.{KnowsBinTupTupTypes, ScenarioParams}
import fun.gravax.distrib.struct.BinData
import zio.dynamodb.PartitionKeyExpression.PartitionKey
import zio.dynamodb.ProjectionExpression.{$, Unknown}
import zio.dynamodb.SortKeyExpression.SortKey
import zio.dynamodb.{Item, KeyConditionExpression, LastEvaluatedKey, PartitionKeyExpression, PrimaryKey, ProjectionExpression, ReturnConsumedCapacity, ReturnItemCollectionMetrics, SortKeyExpression, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, RIO, ZIO}

private trait BinLoadStuff

trait BinWalker extends KnowsBinItem with KnowsBinTupTupTypes {
	protected def getBinStoreApi: BinStoreApi

	lazy private val myBinStoreApi = getBinStoreApi

	def loadRootBinOnly(scenParms: ScenarioParams): RIO[ZDynDBExec, Option[BinData]] = {
		val rootBinPK: PrimaryKey = ???
		val binDataRdOp = myBinStoreApi.readBinData(rootBinPK)
		binDataRdOp
	}

	val maxBinKeyResultSize = 10

	def queryOp4BinScalars(scenParms: ScenarioParams): RIO[ZDynDBExec, (Chunk[Item], LastEvaluatedKey)]  = {

		val scalarFields = List(KEYNM_PART_SCENARIO, KEYNM_SORT_COMPOUND, FLDNM_TIME_OBS, FLDNM_TIME_PRED, FLDNM_TIME_CALC, FLDNM_BIN_TAG,
		FLDNM_PARENT_TAG, FLDNM_BIN_FLAVOR, FLDNM_BIN_MASS, FLDNM_BIN_REL_WEIGHT, FLDNM_BIN_ABS_WEIGHT)

	// 	val attrPrjNmMap = SMap[String,String]("#timeObs" -> FLDNM_TIME_OBS, "#timePred" -> FLDNM_TIME_PRED)

		println(s"println scalarFieldnames = ${scalarFields}")

		lazy val scalarProjList: Seq[ProjectionExpression[Any, ProjectionExpression.Unknown]] = scalarFields.map(fldNm => {
			projExpr(fldNm)
		})
		println(s"println scalarProjList = ${scalarProjList}")

		val sortKeyPrefix = scenParms.sortKeyQryPrefix
		val tblNm = scenParms.getTgtTblNm
		val scenID = scenParms.getScenID

		val baseQry = ZDynDBQry.querySomeItem(tblNm, limit = maxBinKeyResultSize,  scalarProjList : _*)
		println(s"println baseQry=${baseQry}")
		val partKeyExpr: PartitionKeyExpression = PartitionKey(KEYNM_PART_SCENARIO).===(scenID)
		println(s"partKeyExpr = ${partKeyExpr}")
		val sortKeyExpr: SortKeyExpression = SortKey(KEYNM_SORT_COMPOUND).beginsWith(sortKeyPrefix)
		println(s"sortKeyExpr = ${sortKeyExpr}")
		val fullKeyExpr: KeyConditionExpression = partKeyExpr && sortKeyExpr
		println(s"println: queryOp4BinScalars fullKeyExpr=${fullKeyExpr}")
		val qryWW: ZDynDBQry[Any, (Chunk[Item], LastEvaluatedKey)] = baseQry.whereKey(fullKeyExpr)
		// val countOp: ZIO[ZDynDBExec, Throwable, (Chunk[Item], LastEvaluatedKey)] = qryWW.selectCount.execute
		val qryWCap = qryWW.capacity(ReturnConsumedCapacity.Total)
		val qryWMet = qryWCap.metrics(ReturnItemCollectionMetrics.Size)
		val qryOp: RIO[ZDynDBExec, (Chunk[Item], LastEvaluatedKey)] = qryWMet.execute // .debug
		qryOp
	}

	def extractBinScalarsFromQRsltItems(qrsltItems : Chunk[Item]): Chunk[BinScalarInfoTup] = {
		val ebdChnk: Chunk[BinScalarInfoTup] = qrsltItems.map(itm => myBinStoreApi.myFBI.extractBinScalars(itm))
		ebdChnk
	}

	def fetchMeatyBinItems(scenParms: ScenarioParams, binInfoTups : Seq[BinScalarInfoTup]): RIO[ZDynDBExec, List[Option[Item]]] = {
		// We may batch fetch up to 100 items, within a total of 16MB, which would allow ~160K average per item.
		// BatchGetItem() is private, but BatchGetItemExamples shows how to build up getItem requests that
		// are supposed to magically become BatchGetItem
		val batchGet: ZDynDBQry[Any, List[Option[Item]]] = ZDynDBQry.forEach(binInfoTups) { binfTup =>
			qryForOneMeatyBinItem(scenParms, binfTup)
		}
		val bgExec: ZIO[ZDynDBExec, Throwable, List[Option[Item]]] = batchGet.execute
		bgExec
	}
	def shamWow(scenParms: ScenarioParams, binInfoTups : Seq[BinScalarInfoTup]) = {
		val firstTup = binInfoTups.head
		val qry = qryForOneMeatyBinItem(scenParms, firstTup)
		qry.execute
	}
	def qryForOneMeatyBinItem(scenParms: ScenarioParams, binfTup : BinScalarInfoTup): ZDynDBQry[Any, Option[Item]]  = {
		val (timeInf, tagInf, massInf) = binfTup
		val sortKey = scenParms.exactSortKey(timeInf, tagInf)
		val tblNm = scenParms.getTgtTblNm
		val scenID = scenParms.getScenID
		val itemPK = PrimaryKey(KEYNM_PART_SCENARIO -> scenID, KEYNM_SORT_COMPOUND -> sortKey)
		val gitmQry: ZDynDBQry[Any, Option[Item]] = qryForOneMeatyBinItemAtPK(tblNm, itemPK)
		gitmQry
	}
	def qryForOneMeatyBinItemAtPK(tblNm : String, itemPK : PrimaryKey) : ZDynDBQry[Any, Option[Item]] = {
		// Adding any field projections to the getItem call makes the eventual DB fetch fail (returns None).
		// Tried several variations but no luck yet.
		// Anyway in our current design the "full item" is not much larger than just "the meat", so we are pushing
		// ahead without the projections.
		val projList: Seq[ProjectionExpression[Any, Unknown]] = List(projExpr(FLDNM_BIN_TAG), projExpr(FLDNM_BIN_MASS))
		val gitmQry = ZDynDBQry.getItem(tblNm, itemPK) // , projList : _*)  // ,			$(FLDNM_BIN_TAG)
		// $(KEYNM_SORT_COMPOUND),	$(FLDNM_BIN_TAG), $(FLDNM_BIN_MASS), $(FLDNM_BIN_FLAVOR), $(FLDNM_DOBLE_MAP)
	    //  where $("field1") === 42
		println(s"qryForOneMeatyBinItemAtPK: ${gitmQry}")
		gitmQry
	}

	//  --projection-expression "Description, RelatedItems[0], ProductReviews.FiveStar"

	// .parse returns an Either
	// .$ will throw on failure
	def projExpr(expr: String) = {
		// import zio.dynamodb._
		// import zio.dynamodb.DynamoDBQuery._
		ProjectionExpression.$(expr)
	}
	def projExprParse(expr: String) = ProjectionExpression.parse(expr)

	def codeCopiedFromBatchGetItemExample = {
		// From   zio.dynamodb.examples.BatchGetItemExamples
		// If we have an Iterable of data from which we wish to create a batch query from we can use `DynamoDBQuery.forEach`
		// The below example will create 1 BatchGetItem containing 10 GetItem requests
		val batchFromIterable: ZDynDBQry[Any, List[Option[Item]]] = ZDynDBQry.forEach(1 to 10) { i =>
			ZDynDBQry.getItem(
				"T1",
				PrimaryKey("field1" -> i),
				$("field1"),
				$("field2")
			) where $(	"field1" ) === 42
		}
	}
}
	/*
	val querySome: ZIO[DynamoDBExecutor, Throwable, (Chunk[Item], LastEvaluatedKey)] =
		ZDynDBQry.querySomeItem("tableName1", limit = 10, $("A"), $("B"), $("C"))
				.sortOrder(ascending = false)
				.whereKey(PartitionKey("partitionKey1") === "x" && SortKey("sortKey1") > "X")
				.selectCount
				.execute

  def querySomeItem(tableName: String, limit: Int, projections: ProjectionExpression[_, _]*): QuerySome =
    QuerySome(
      TableName(tableName),
      limit,
      select = selectOrAll(projections),
      projections = projections.toList
    )

  private def selectOrAll(projections: Seq[ProjectionExpression[_, _]]): Option[Select] =
    Some(if (projections.isEmpty) Select.AllAttributes else Select.SpecificAttributes)

  private def awsQueryRequest(querySome: QuerySome): QueryRequest = {
    val (aliasMap, (maybeFilterExpr, maybeKeyExpr)) = (for {
      filter  <- AliasMapRender.collectAll(querySome.filterExpression.map(_.render))
      keyExpr <- AliasMapRender.collectAll(querySome.keyConditionExpression.map(_.render))
    } yield (filter, keyExpr)).execute
    val mapAndExprn                                 = awsExprnAttrNamesAndReplacedFn(maybeFilterExpr)(ZIOAwsConditionExpression(_))
    val maybeProjectionExpressions                  = toOption(querySome.projections).map(awsProjectionExpression)
    val mapAndProjectionExprn                       =
      awsExprnAttrNamesAndReplacedFn(maybeProjectionExpressions)(ZIOAwsProjectionExpression(_))
    val awsNamesMap                                 = mergeOptionalMaps(mapAndExprn._1, mapAndProjectionExprn._1)

    QueryRequest(
      tableName = ZIOAwsTableName(querySome.tableName.value),
      indexName = querySome.indexName.map(_.value).map(ZIOAwsIndexName(_)),
      select = querySome.select.map(awsSelect),
      limit = Some(querySome.limit).map(PositiveIntegerObject(_)),
      consistentRead = Some(toBoolean(querySome.consistency)).map(ZIOAwsConsistentRead(_)),
      scanIndexForward = Some(querySome.ascending),
      exclusiveStartKey = querySome.exclusiveStartKey.map(m => awsAttributeValueMap(m.map)),
      returnConsumedCapacity = Some(awsConsumedCapacity(querySome.capacity)),
      projectionExpression = mapAndProjectionExprn._2,
      filterExpression = mapAndExprn._2,
      expressionAttributeValues = aliasMapToExpressionZIOAwsAttributeValues(aliasMap).map(_.map {
        case (k, v) => (ZIOAwsExpressionAttributeValueVariable(k), v)
      }),
      expressionAttributeNames = awsNamesMap,
      keyConditionExpression = maybeKeyExpr.map(ZIOAwsKeyExpression(_))
    )
  }

You can use any attribute name in a projection expression, provided that the first character is a-z or A-Z and the
second character (if present) is a-z, A-Z, or 0-9.

If an attribute name contains a dot (".") or a hyphen ("-"), you must use an expression attribute name to replace
that attribute's name in the expression.

An expression attribute name is a placeholder that you use in an Amazon DynamoDB expression as an alternative to an
actual attribute name. An expression attribute name must begin with a pound sign (#), and be followed by one or
more alphanumeric characters.

If you need to compare an attribute with a value, define an expression attribute value as a placeholder.
Expression attribute values in Amazon DynamoDB are substitutes for the actual values that you want to compare—
values that you might not know until runtime. An expression attribute value must begin with a colon (:) and be
followed by one or more alphanumeric characters.


I am enjoying the zio-dynamodb library!
I see that the previous topic in this chat was closely related, but I still haven't found the code I am looking for.

In my calls to querySomeItem I would like to use projection expression name aliases, such as
  "#shortnm" -> "name-with-dashes".  I see these mentioned in the source code for ProjectionExpression.scala and down in the deeper guts, but I haven't found an example of how to plug in my map of String expression name aliases while building up the query.
To avoid any ambiguity, I am talking about THESE puppies:
https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.ExpressionAttributeNames.html


        test("batch get item") {
          withDefaultTable { tableName =>
            val getItems = BatchGetItem().addAll(
              GetItem(TableName(tableName), pk(avi3Item)),
              GetItem(TableName(tableName), pk(adam2Item))
            )
            for {
              a <- getItems.execute
            } yield assert(a)(
              equalTo(
                BatchGetItem.Response(
                  responses = MapOfSet.apply(
                    ScalaMap[TableName, Set[Item]](
                      TableName(tableName) -> Set(avi3Item, adam2Item)
                    )
                  )
                )

For applications that need to read or write multiple items, DynamoDB provides the BatchGetItem and BatchWriteItem
operations. Using these operations can reduce the number of network round trips from your application to DynamoDB.
In addition, DynamoDB performs the individual read or write operations in parallel. Your applications benefit from
this parallelism without having to manage concurrency or threading.

In general, a batch operation does not fail unless all the requests in the batch fail. For example, suppose that you
perform a BatchGetItem operation, but one of the individual GetItem requests in the batch fails. In this case,
BatchGetItem returns the keys and data from the GetItem request that failed. The other GetItem requests in the batch
are not affected.

A single BatchGetItem operation can contain up to 100 individual GetItem requests and can retrieve up to 16 MB of data.
In addition, a BatchGetItem operation can retrieve items from multiple tables.


	*/
