package fun.gravax.zdynamo

import zio.{Chunk, RIO, ZIO}
import zio.dynamodb.PartitionKeyExpression.PartitionKey
import zio.dynamodb.ProjectionExpression.{Unknown, builder}
import zio.dynamodb.SortKeyExpression.SortKey
import zio.dynamodb.{Item, KeyConditionExpression, LastEvaluatedKey, PartitionKeyExpression, PrimaryKey, ProjectionExpression, ReturnConsumedCapacity, ReturnItemCollectionMetrics, SortKeyExpression, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}

import scala.collection.immutable.{Queue, Map => SMap}

private trait BinLoadStuff

trait BinWalker extends KnowsBinItem {
	protected def getBinStoreApi: BinStoreApi

	lazy private val myBinStoreApi = getBinStoreApi

	def loadRootBinOnly(scenParms: ScenarioParams): RIO[ZDynDBExec, Option[BinData]] = {
		val rootBinPK: PrimaryKey = ???
		val binDataRdOp = myBinStoreApi.readBinData(rootBinPK)
		binDataRdOp
	}

	val maxBinKeyResultSize = 500

	def queryOp4BinScalars(scenParms: ScenarioParams): RIO[ZDynDBExec, (Chunk[Item], LastEvaluatedKey)]  = {
		/*

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

		 */
		val scalarFields = List(KEYNM_PART_SCENARIO, KEYNM_SORT_COMPOUND, FLDNM_TIME_OBS, FLDNM_TIME_PRED, FLDNM_TIME_CALC, FLDNM_BIN_TAG,
		FLDNM_PARENT_TAG, FLDNM_BIN_FLAVOR, FLDNM_BIN_MASS, FLDNM_BIN_REL_WEIGHT, FLDNM_BIN_ABS_WEIGHT)

		import zio.dynamodb._
		import zio.dynamodb.DynamoDBQuery._
		import ProjectionExpression.$


	// 	val attrPrjNmMap = SMap[String,String]("#timeObs" -> FLDNM_TIME_OBS, "#timePred" -> FLDNM_TIME_PRED)

		println(s"println scalarFieldnames = ${scalarFields}")
		// val proj01 = projExpr(FLDNM_TIME_OBS)
		// println(s"println proj01=${proj01}")
		// val proj02 = projExpr(FLDNM_BIN_TAG)
		lazy val scalarProjList: Seq[ProjectionExpression[Any, Unknown]] = scalarFields.map(fldNm => {
			$(fldNm)
		})
		// println(s"println scalarProjList = ${scalarProjList}")

		// projExpr(FLDNM_BIN_REL_WEIGHT)
		// projExpr(FLDNM_BIN_REL_WEIGHT)
		// projExpr(FLDNM_BIN_MASS), projExpr(FLDNM_BIN_ABS_WEIGHT)
		val sortKeyPrefix = scenParms.sortKeyQryPrefix
		val tblNm = scenParms.getTgtTblNm
		val scenID = scenParms.getScenID
		// $("justjunk")) // , $("#myobs")) //
		val baseQry = ZDynDBQry.querySomeItem(tblNm, limit = maxBinKeyResultSize,  scalarProjList : _*) // proj01, proj02)
		// val bwn = baseQry.selectSpecificAttributes
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

	//  --projection-expression "Description, RelatedItems[0], ProductReviews.FiveStar"

	// .parse returns an Either
	// .$ will throw on failure
	def projExpr(expr: String) = ProjectionExpression.$(expr)
	def projExprParse(expr: String) = ProjectionExpression.parse(expr)
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


    QueryRequest(
      tableName = ZIOAwsTableName(queryAll.tableName.value),
      indexName = queryAll.indexName.map(_.value).map(ZIOAwsIndexName(_)),
      select = queryAll.select.map(awsSelect),
      limit = queryAll.limit.map(PositiveIntegerObject(_)),
      consistentRead = Some(toBoolean(queryAll.consistency)).map(ZIOAwsConsistentRead(_)),
      scanIndexForward = Some(queryAll.ascending),
      exclusiveStartKey = queryAll.exclusiveStartKey.map(m => awsAttributeValueMap(m.map)),
      projectionExpression = mapAndProjectionExprn._2,
      returnConsumedCapacity = Some(awsConsumedCapacity(queryAll.capacity)),
      filterExpression = mapAndExprn._2,
      expressionAttributeValues = aliasMapToExpressionZIOAwsAttributeValues(aliasMap).map(m =>
        m.map { case (k, v) => (ZIOAwsExpressionAttributeValueVariable(k), v) }
      ),
      expressionAttributeNames = awsNamesMap,
      keyConditionExpression = maybeKeyExpr.map(ZIOAwsKeyExpression(_))
    )


I am enjoying the zio-dynamodb library!
I see that the previous topic in this chat was closely related, but I still haven't found the code I am looking for.

In my calls to querySomeItem I would like to use projection expression name aliases, such as
  "#shortnm" -> "name-with-dashes".  I see these mentioned in the source code for ProjectionExpression.scala and down in the deeper guts, but I haven't found an example of how to plug in my map of String expression name aliases while building up the query.
To avoid any ambiguity, I am talking about THESE puppies:
https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.ExpressionAttributeNames.html


	*/
