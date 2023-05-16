package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.{BinWalker, KnowsBinItem}
import fun.gravax.distrib.gen.{KnowsBinTupTupTypes, ScenarioParams}
import zio.dynamodb.{DynamoDBExecutor, Item, LastEvaluatedKey}
import zio.{Chunk, RIO}

trait BinTreeStuff {

}

trait BinTreeLoader extends KnowsBinItem with KnowsBinTupTupTypes {
	protected def getBinWalker : BinWalker
	lazy private val myBinWalker = getBinWalker

	def loadBinTree(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int, maxBins : Int): RIO[DynamoDBExecutor, Chunk[BinScalarInfoTup]] = {
		// This can return any number of records as long as the total data returned is < 1 MB.
		val fetchSclrItemsOp: RIO[DynamoDBExecutor, (Chunk[Item], LastEvaluatedKey)] = myBinWalker.queryOp4BinScalars(scenParms)

		val fetchSclrTupsOp: RIO[DynamoDBExecutor, Chunk[BinScalarInfoTup]] = fetchSclrItemsOp.map(rsltPair =>
				myBinWalker.extractBinScalarsFromQRsltItems(rsltPair._1))

		fetchSclrTupsOp

		// meatyBinItems <- myBinWalker.fetchMeatyBinItems(fixedScenPrms, sclrTups)

		// At this point we can look at the Tags to determine how many levels we are working with.
		// We could build up a BinNode structure that can be optionally eager or lazy.

		// We can ony fetch up to 100 items in each meat-fetch (fewer if they are very big).  Batch data total < 16MB.
		// [unless there is automatic multi-batching in the ZDynDBQry.  (No reason to think there is, but COULD BE).]
		// So to "fetch any number" we would want a Stream.
		// But perhaps for now we will be content to just fetch blocks of child nodes.

	}

	def buildTree(binScInfTupChnk : Chunk[BinScalarInfoTup]) = {

	}

	def mkBinDataRecAndNode = ???
}
