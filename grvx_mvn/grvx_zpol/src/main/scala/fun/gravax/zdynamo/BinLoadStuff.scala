package fun.gravax.zdynamo

import zio.RIO
import zio.dynamodb.{ Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}

private trait BinLoadStuff

trait BinWalker {
	protected def getBinStoreApi : BinStoreApi
	lazy private val myBinStoreApi = getBinStoreApi
	def loadRootBinOnly(scenParms : ScenarioParams) : RIO[ZDynDBExec, Option[BinData]]   = {
		val rootBinPK : PrimaryKey = ???
		val binDataRdOp = myBinStoreApi.readBinData(rootBinPK)
		binDataRdOp
	}
}
