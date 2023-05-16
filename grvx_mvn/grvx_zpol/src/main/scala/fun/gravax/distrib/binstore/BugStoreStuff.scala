package fun.gravax.distrib.binstore

import zio.dynamodb.{Item, PrimaryKey, ProjectionExpression, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{RIO, Task, TaskLayer, ZIO, ZIOAppDefault, dynamodb => ZDyn}

private trait  BugStoreStuff

object MinBugStore extends ZIOAppDefault {
	val KEYNM_PARTITION = "bugPartKey"
	val KEYNM_SORT = "bugSortKey"

	val scenAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_PARTITION)
	// Examples seen so far use Strings for date values.
	val sortKeyAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_SORT)

	val FLDNM_BUGGY_TAG = "bugTag"
	val FLDNM_BUGGY_FLAVOR = "bugFlavor"
	val FLDNM_BUGGY_MASS = "bugMass"

	// TODO: Check for changes when tableName has hyphen
	val buggyTblNm = "bug-juice"
	val (flg_doCreate, flg_doDelete) = (true, true)

	val binKeySchm = ZDyn.KeySchema(KEYNM_PARTITION, KEYNM_SORT)

	def maybeCreateBugTable: RIO[ZDynDBExec, Unit] = {
		if (flg_doCreate) {
			ZDynDBQry.createTable(buggyTblNm, binKeySchm, ZDyn.BillingMode.PayPerRequest)(
				scenAttr, sortKeyAttr).execute *> ZIO.log(s"Created table ${buggyTblNm}")
		} else ZIO.succeed()
	}
	def maybeDeleteBugTable: RIO[ZDynDBExec, Unit] = if (flg_doDelete) {
		ZDynDBQry.deleteTable(buggyTblNm).execute *> ZIO.log(s"Deleted table ${buggyTblNm}")
	}  else ZIO.succeed()
	val (scn_01, sort_AA) = ("scn_bug_01", "sort_bug_AA" )
	val bugItm01AA : Item = Item(
		KEYNM_PARTITION	-> scn_01,
		KEYNM_SORT -> sort_AA,
		FLDNM_BUGGY_TAG -> "tag 01-AA",
		FLDNM_BUGGY_FLAVOR -> "coconut",
		FLDNM_BUGGY_MASS -> BigDecimal("12.144"))
	val primaryKey01AA = PrimaryKey(KEYNM_PARTITION -> scn_01, KEYNM_SORT -> sort_AA)
	def putOneBugItem : RIO[ZDynDBExec, Unit] = {
		val zpi: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(buggyTblNm, bugItm01AA)
		val zpiEx: ZIO[ZDynDBExec, Throwable, Option[Item]] = zpi.execute
		zpiEx.flatMap(opt_itm_out => ZIO.log(s"s putOneBugItem[${buggyTblNm}] result: ${opt_itm_out}"))
	}
	def getOneFullBugItem : RIO[ZDynDBExec, Unit] = {
		// Plain .getItem works and returns all fields, as expected.
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(buggyTblNm, primaryKey01AA)
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneFullBugItem[${buggyTblNm}] result: ${opt_itm_out}"))
	}
	def projExpr(expr: String) = {
		ProjectionExpression.$(expr)
	}
	def getOneBugItemAllFields : RIO[ZDynDBExec, Unit] = {
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(buggyTblNm, primaryKey01AA,
			// .getItem with *all* fields specified in projection expressions works as expected.
			projExpr(KEYNM_PARTITION), projExpr(KEYNM_SORT), projExpr(FLDNM_BUGGY_FLAVOR),
			projExpr(FLDNM_BUGGY_TAG), projExpr(FLDNM_BUGGY_MASS))
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneBugItemAllFields[${buggyTblNm}] result: ${opt_itm_out}"))
	}
	def getOneBugItemFewerFields : RIO[ZDynDBExec, Unit] = {
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(buggyTblNm, primaryKey01AA,
			// If the .getItem projections includes both key fields (partition + sort) then .getItem works fine
			// However if either key is left out of the projection list, then .getItem will return None.
			projExpr(KEYNM_PARTITION), projExpr(KEYNM_SORT),
			projExpr(FLDNM_BUGGY_FLAVOR), projExpr(FLDNM_BUGGY_TAG))
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneBugItemFewerFields[${buggyTblNm}] result: ${opt_itm_out}"))
	}
	def getOneBugItemWithoutKeys_FAILS : RIO[ZDynDBExec, Unit] = {
		val zgi : ZDynDBQry[Any, Option[Item]] = ZDynDBQry.getItem(buggyTblNm, primaryKey01AA,
			// Because keys are missing from the list of projection expressions, this call to .getItem returns None.
			projExpr(FLDNM_BUGGY_FLAVOR), projExpr(FLDNM_BUGGY_TAG))
		val zgiEx = zgi.execute
		zgiEx.flatMap(opt_itm_out => ZIO.log(s"s getOneBugItemWithoutKeys_FAILS[${buggyTblNm}] result: ${opt_itm_out}"))
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