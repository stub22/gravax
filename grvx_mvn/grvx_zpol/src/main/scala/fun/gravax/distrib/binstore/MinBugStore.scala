package fun.gravax.distrib.binstore

import zio.dynamodb.{Item, PrimaryKey, ProjectionExpression, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{RIO, Task, TaskLayer, ZIO, ZIOAppDefault, dynamodb => ZDyn}

object MinBugStore extends ZIOAppDefault {
	val KEYNM_PARTITION = "bugPartKey"
	val KEYNM_SORT = "bugSortKey"

	val bugJuiceKeySchema = ZDyn.KeySchema(KEYNM_PARTITION, KEYNM_SORT)

	val partKeyAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_PARTITION)
	val sortKeyAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_SORT)

	// Field names for non-key attributes
	val FLDNM_BUGGY_TAG = "bugTag"
	val FLDNM_BUGGY_FLAVOR = "bugFlavor"
	val FLDNM_BUGGY_MASS = "bugMass"

	// TODO: Look for behavior differences when tableName contains special characters, such as '-' hyphen.
	val TBLNM_BUGGY = "bug-juice"
	val (flg_doCreateTable, flg_doDeleteTable) = (true, true)

	// Data values for a single test record.  Here are the key values.
	val (partKey_01, sortKey_AA) = ("part_bug_01", "sort_bug_AA" )

	// Here is a full item value, followed by the primaryKey for the same Item.
	val bugItm01AA : Item = Item(
		KEYNM_PARTITION	-> partKey_01,
		KEYNM_SORT -> sortKey_AA,
		FLDNM_BUGGY_TAG -> "tag 01-AA",
		FLDNM_BUGGY_FLAVOR -> "coconut",
		FLDNM_BUGGY_MASS -> BigDecimal("12.144"))
	val primaryKey01AA = PrimaryKey(KEYNM_PARTITION -> partKey_01, KEYNM_SORT -> sortKey_AA)

	def maybeCreateBugTable: RIO[ZDynDBExec, Unit] = {
		if (flg_doCreateTable) {
			ZDynDBQry.createTable(TBLNM_BUGGY, bugJuiceKeySchema, ZDyn.BillingMode.PayPerRequest)(
				partKeyAttr, sortKeyAttr).execute *> ZIO.log(s"Created table ${TBLNM_BUGGY}")
		} else ZIO.succeed()
	}
	def maybeDeleteBugTable: RIO[ZDynDBExec, Unit] = if (flg_doDeleteTable) {
		ZDynDBQry.deleteTable(TBLNM_BUGGY).execute *> ZIO.log(s"Deleted table ${TBLNM_BUGGY}")
	}  else ZIO.succeed()

	def putOneBugItem : RIO[ZDynDBExec, Unit] = {
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(TBLNM_BUGGY, bugItm01AA)
		val zpiEx: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiEx.flatMap(opt_itm_out => ZIO.log(s"s putOneBugItem[${TBLNM_BUGGY}] result: ${opt_itm_out}"))
	}

	def getOneFullBugItem : RIO[ZDynDBExec, Unit] = {
		// Plain .getItem works and returns all fields, as expected.
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(TBLNM_BUGGY, primaryKey01AA)
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneFullBugItem[${TBLNM_BUGGY}] result: ${opt_itm_out}"))
	}

	def projExpr(expr: String) = {
		ProjectionExpression.$(expr)
	}

	def getOneBugItemAllFields : RIO[ZDynDBExec, Unit] = {
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(TBLNM_BUGGY, primaryKey01AA,
			// .getItem with *all* fields specified in projection expressions works as expected.
			projExpr(KEYNM_PARTITION), projExpr(KEYNM_SORT), projExpr(FLDNM_BUGGY_FLAVOR),
			projExpr(FLDNM_BUGGY_TAG), projExpr(FLDNM_BUGGY_MASS))
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneBugItemAllFields[${TBLNM_BUGGY}] result: ${opt_itm_out}"))
	}

	def getOneBugItemFewerFields : RIO[ZDynDBExec, Unit] = {
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(TBLNM_BUGGY, primaryKey01AA,
			// If the .getItem projections includes both key fields (partition + sort) then .getItem works fine.
			// However if either key is left out of the projection list, then .getItem will return None.
			projExpr(KEYNM_PARTITION), projExpr(KEYNM_SORT),
			projExpr(FLDNM_BUGGY_FLAVOR), projExpr(FLDNM_BUGGY_TAG))
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneBugItemFewerFields[${TBLNM_BUGGY}] result: ${opt_itm_out}"))
	}

	def getOneBugItemWithoutKeys_FAILS : RIO[ZDynDBExec, Unit] = {
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(TBLNM_BUGGY, primaryKey01AA,
			// Because keys are missing from the list of projection expressions, this call to .getItem returns None.
			projExpr(FLDNM_BUGGY_FLAVOR), projExpr(FLDNM_BUGGY_TAG))
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneBugItemWithoutKeys_FAILS[${TBLNM_BUGGY}] result: ${opt_itm_out}"))
	}

	def mkProgram: RIO[ZDynDBExec, Unit] = {
		for {
			_ <- maybeCreateBugTable
			_ <- putOneBugItem
			_ <- getOneFullBugItem
			_ <- getOneBugItemAllFields
			_ <- getOneBugItemFewerFields
			_ <- getOneBugItemWithoutKeys_FAILS
			_ <- maybeDeleteBugTable
		} yield()
	}

	override def run: Task[Unit] = {
		val program = mkProgram
		val dbExecLayer: TaskLayer[ZDynDBExec] = LocalDynamoDB.layer
		program.provide(dbExecLayer)
	}

}