package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.{BinWalker, KnowsBinItem}
import fun.gravax.distrib.gen.{KnowsBinTupTupTypes, KnowsGenTypes, ScenarioParams}
import zio.dynamodb.{Item, LastEvaluatedKey, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.stream.ZStream
import zio.{Chunk, IO, RIO, Task, ZIO}

import scala.collection.immutable.{Map => SMap}


trait BinTreeStuff {

}

trait BinTreeLoader extends KnowsBinItem with KnowsBinTupTupTypes with StatTupleShapes with KnowsGenTypes {
	protected def getBinWalker : BinWalker
	lazy private val myBinWalker = getBinWalker



	def loadBinTreeEagerly(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int, maxBins : Int):
					RIO[ZDynDBExec, Chunk[(BinScalarInfoTup, BinMeatInfo)]] = {

		val tupStrm = scalarTupStream(scenParms, maxLevels, maxBins)
		// FIXME:  This join step is too eager.  We want our tree to be lazier and smarter than this.
		val meatyPairStrm =  joinMeatToBinScalars(meatCache)(scenParms, maxLevels)(tupStrm)
		val meatyPairChnk: ZIO[ZDynDBExec, Throwable, Chunk[(BinScalarInfoTup, BinMeatInfo)]] = meatyPairStrm.runCollect
		meatyPairChnk

		// At this point we can look at the Tags to determine how many levels we are working with.
		// We could build up a BinNode structure that can be optionally eager or lazy.

		// We can ony fetch up to 100 items in each meat-fetch (fewer if they are very big).  Batch data total < 16MB.
		// [unless there is automatic multi-batching in the ZDynDBQry.  (No reason to think there is, but COULD BE).]
		// So to "fetch any number" we would want a Stream.
		// But perhaps for now we will be content to just fetch blocks of child nodes.

	}

	def scalarTupStream(scenParms: ScenarioParams, maxLevels : Int, maxBins : Int): ZStream[ZDynDBExec, Throwable, BinScalarInfoTup] = {
		// This can return any number of records as long as the total data returned is < 1 MB.
		val fetchSclrItemsOp: RIO[ZDynDBExec, (Chunk[Item], LastEvaluatedKey)] = myBinWalker.queryOp4BinScalars(scenParms, maxBins)

		val fetchSclrTupsOp: RIO[ZDynDBExec, Chunk[BinScalarInfoTup]] = fetchSclrItemsOp.map(rsltPair =>
			myBinWalker.extractBinScalarsFromQRsltItems(rsltPair._1))

		val chnkStrm: ZStream[ZDynDBExec, Throwable, Chunk[BinScalarInfoTup]] = ZStream.fromZIO(fetchSclrTupsOp)
		val tupStrm: ZStream[ZDynDBExec, Throwable, BinScalarInfoTup] = chnkStrm.flattenChunks
		tupStrm
	}

	def joinMeatToBinScalars(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int)
							(binScInfTupStrm : ZStream[ZDynDBExec, Throwable, BinScalarInfoTup]):
					ZStream[ZDynDBExec, Throwable, (BinScalarInfoTup, BinMeatInfo)] = {
		// During this stream run, we force all meat to be loaded.
		// TODO:  Enforce maxLevels.
		val strmWithMeat: ZStream[ZDynDBExec, Throwable, (BinScalarInfoTup, BinMeatInfo)] = binScInfTupStrm.mapZIO(binfTup => {
			val (timeInf, tagInf, massInf) = binfTup
			val binKeyInfo = scenParms.mkFullBinKey(timeInf, tagInf)
			val meatFetchOp : Task[Option[BinMeatInfo]] = meatCache.get(binKeyInfo)
			// By calling optMeat.get, we insist that meat-fetch must work, otherwise this stream should fail.
			val pairedWithMeatOp: Task[(BinScalarInfoTup, BinMeatInfo)] = meatFetchOp.map(optMeat => (binfTup, optMeat.get))
			pairedWithMeatOp
		})
		strmWithMeat
	}

	def loadBinTreeLazily(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int, maxBins : Int): RIO[ZDynDBExec, BinNode] = {
		val tupStrm = scalarTupStream(scenParms, maxLevels, maxBins)
		val skelIndexMapOp = buildIndexMap(tupStrm)

		val binTreeOp: RIO[ZDynDBExec, BinNode] = skelIndexMapOp.map(skelIdxMap => {
			val rootTag = findRootTag(skelIdxMap)
			buildBinSubtree(meatCache)(scenParms, maxLevels)(skelIdxMap, rootTag)
		})
		binTreeOp
	}

	def findRootTag(idxMap : SMap[BinTag, SkelNode]): BinTag = {
		val tagInfos: Iterable[BinTagInfo] = idxMap.values.map(_.infoTup_opt.get._2)

		val possibleRoots: Iterable[BinTagInfo] = tagInfos.filter(_.parentTag == PTAG_NO_PARENT)
		assert(possibleRoots.size == 1)
		possibleRoots.head.binTag
	}

	// Optional info-tup allows us to construct nodes before we have seen their records, which allows us to process
	// parents+children in any order.
	case class SkelNode(kids : Vector[BinTag], infoTup_opt : Option[BinScalarInfoTup]) {
		def addKid(kidTag : BinTag) : SkelNode = SkelNode(kids :+ kidTag, infoTup_opt)
		def setInf(info : BinScalarInfoTup) : SkelNode = SkelNode(kids, Some(info))
	//	def getParentTag_opt : Option[BinTag] = ??? // if(parentTag == PTAG_NO_PARENT) None else Some(parentTag)
	}

	// Build map of SkelNodes, where each contains a collection of child-tags.
	// TODO:  Test that this always works, regardless of order of the input records.
	def buildIndexMap (binScInfTupStrm : ZStream[ZDynDBExec, Throwable, BinScalarInfoTup])
					: RIO[ZDynDBExec, SMap[BinTag, SkelNode]] = {
		val initSkelNodeMap = SMap[BinTag, SkelNode]()
		val idxMapOp : RIO[ZDynDBExec, SMap[BinTag, SkelNode]] = binScInfTupStrm.runFold(initSkelNodeMap)((prevMap, nxtTup) => {
			val childTag = nxtTup._2.binTag
			val parentTag = nxtTup._2.parentTag
			val ptag_opt = if(parentTag == PTAG_NO_PARENT) None else Some(parentTag)
			val mapWithChild: SMap[BinTag, SkelNode] = prevMap.updatedWith(childTag)(oldChildOpt => oldChildOpt match {
				case None => Some(SkelNode(Vector(), Some(nxtTup)))
				case Some(oldChild) => Some(oldChild.setInf(nxtTup))
			})
			val upMap = ptag_opt match {
				case None => mapWithChild
				case Some(ptag : BinTag) => mapWithChild.updatedWith(ptag)(oldParentNodeOpt => {
					Some(oldParentNodeOpt.fold(SkelNode(Vector(), None))(oldParent => oldParent.addKid(childTag)))
				})
			}
			upMap
		})
		idxMapOp
	}

	// To make a tree (where immutable nodes point to immutable children) we want to *complete* the leaf nodes *first*.
	// One way to do this is to proceed recursively from the unfinished-root.
	def buildBinSubtree(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int)
								(skelNodeMap : SMap[BinTag, SkelNode], topTag : BinTag): BinNode = {

		val topSkelNode = skelNodeMap.get(topTag).get
		val kidTags: Seq[BinTag] = topSkelNode.kids
		val kidSubtrees : Seq[BinNode] = if (maxLevels >= 2) {
			kidTags.map(ktag => buildBinSubtree(meatCache)(scenParms, maxLevels -1)(skelNodeMap, ktag))
		} else Seq()

		val topInfoTup = topSkelNode.infoTup_opt.get
		val (topTimeInfo, topTagInfo, topMassInfo) = topInfoTup
		val topKeyInfo = scenParms.mkFullBinKey(topTimeInfo, topTagInfo)
		val scenID = scenParms.getScenID
		val topBinDat = new CacheBackedBinData {
			override protected def getCache: MeatyItemCache = meatCache
			override protected def getBinKey: BinFullKeyInfo = topKeyInfo
			override protected def getTimeInfo: BinTimeInfo = topTimeInfo
			override protected def getTagInfo: BinTagInfo = topTagInfo
			override protected def getMassInfo: BinMassInfo = topMassInfo

			override def getScenarioID: BinFlavor = scenID
			override def getBinFlavor: BinFlavor = ???

			// override protected def getStatMap: StatMap = ???

			override def mkStatRow(keySeq: IndexedSeq[EntryKey]): Task[StatRow] = ???

			override def allKeysSorted(meatKeyOrder: Ordering[BinFlavor]): IndexedSeq[EntryKey] = ???
		}

		val topBinNode = new BinNode {
			override protected def getBinData: BinData = topBinDat
			override protected def getParent_opt: Option[BinNode] = None

			override protected def getKids: Iterable[BinNode] = kidSubtrees

			override protected def getMeatKeyOrder: Ordering[EntryKey] = ???
		}
		topBinNode
	}

}

