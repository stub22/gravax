package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.{BinWalker, KnowsBinItem}
import fun.gravax.distrib.gen.{KnowsBinTupTupTypes, KnowsGenTypes, ScenarioParams}
import zio.dynamodb.{Item, LastEvaluatedKey, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.stream.ZStream
import zio.{Chunk, IO, Promise, RIO, Task, UIO, ZIO}

import scala.collection.immutable.{Map => SMap}

private trait BinTreeStuff

trait BinTreeLoader extends KnowsBinItem with KnowsBinTupTupTypes with KnowsStatTupleShapes with KnowsGenTypes {
	protected def getBinWalker: BinWalker

	lazy private val myBinWalker = getBinWalker

	protected def scalarTupStream(scenParms: ScenarioParams, maxLevels: Int, maxBins: Int): ZStream[ZDynDBExec, Throwable, BinScalarInfoTup] = {
		// This can return any number of records as long as the total data returned is < 1 MB.
		val fetchSclrItemsOp: RIO[ZDynDBExec, (Chunk[Item], LastEvaluatedKey)] = myBinWalker.queryOp4BinScalars(scenParms, maxBins)

		val fetchSclrTupsOp: RIO[ZDynDBExec, Chunk[BinScalarInfoTup]] = fetchSclrItemsOp.map(rsltPair =>
			myBinWalker.extractBinScalarsFromQRsltItems(rsltPair._1))

		val chnkStrm: ZStream[ZDynDBExec, Throwable, Chunk[BinScalarInfoTup]] = ZStream.fromZIO(fetchSclrTupsOp)
		val tupStrm: ZStream[ZDynDBExec, Throwable, BinScalarInfoTup] = chnkStrm.flattenChunks
		tupStrm
	}
}
trait BinTreeLazyLoader extends BinTreeLoader {
	// Optional info-tup allows us to construct nodes before we have seen their records, which allows us to process
	// parents+children in any order.
	private case class SkelNode(kids : Vector[BinTag], infoTup_opt : Option[BinScalarInfoTup]) {
		def addKid(kidTag : BinTag) : SkelNode = SkelNode(kids :+ kidTag, infoTup_opt)
		def setInf(info : BinScalarInfoTup) : SkelNode = SkelNode(kids, Some(info))
		//	def getParentTag_opt : Option[BinTag] = ??? // if(parentTag == PTAG_NO_PARENT) None else Some(parentTag)
	}

	def loadBinTreeLazily(meatCache : MeatyItemCache, meatKeyOrder : Ordering[EntryKey])(scenParms: ScenarioParams, maxLevels : Int, maxBins : Int): RIO[ZDynDBExec, BinNode] = {
		val tupStrm = scalarTupStream(scenParms, maxLevels, maxBins)
		val skelIndexMapOp = buildIndexMap(tupStrm)

		val binTreeOp: RIO[ZDynDBExec, BinNode] = skelIndexMapOp.flatMap(skelIdxMap => {
			val rootTag = findRootTag(skelIdxMap)
			// We want to create a promise that returns the value none.
			val binNodeOptPromiseMkr: UIO[Promise[Nothing, Option[BinNode]]] = Promise.make[Nothing, Option[BinNode]]
			binNodeOptPromiseMkr.flatMap(parentOptPromise => {
				val psOp: UIO[Boolean] = parentOptPromise.succeed(None)
				psOp.flatMap(psuccResult => {
					buildBinSubtree(meatCache, meatKeyOrder)(scenParms, maxLevels)(skelIdxMap, rootTag, parentOptPromise)
				})
			})
		})
		binTreeOp
	}

	private def findRootTag(idxMap : SMap[BinTag, SkelNode]): BinTag = {
		val tagInfos: Iterable[BinTagInfo] = idxMap.values.map(_.infoTup_opt.get._2)

		val possibleRoots: Iterable[BinTagInfo] = tagInfos.filter(_.parentTag == PTAG_NO_PARENT)
		assert(possibleRoots.size == 1)
		possibleRoots.head.binTag
	}

	// Build map of SkelNodes, where each contains a collection of child-tags.
	// TODO:  Test that this always works, regardless of order of the input records.
	private def buildIndexMap (binScInfTupStrm : ZStream[ZDynDBExec, Throwable, BinScalarInfoTup])
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

	// To wire up the parentNode we need to pass a promise and make the operation effectful, which makes the recursive
	// call a little more elaborate.  An alternative method could be to nest our node-creation inside another object
	// which passes in a method referring to a lazy val.  https://blog.rockthejvm.com/immutable-doubly-linked-list-scala/
	private def buildBinSubtree(meatCache : MeatyItemCache, meatKeyOrder : Ordering[EntryKey])
				(scenParms: ScenarioParams, maxLevels : Int)(skelNodeMap : SMap[BinTag, SkelNode], topTag : BinTag,
				 parentNodeOptPromise: Promise[Nothing, Option[BinNode]]): UIO[BinNode] = {

		// .make  Makes a new promise to be completed by the fiber creating the promise.

		val topSkelNode = skelNodeMap.get(topTag).get
		val kidTags: Seq[BinTag] = topSkelNode.kids
		val binNodeOptPromiseMkr: UIO[Promise[Nothing, Option[BinNode]]] = Promise.make[Nothing, Option[BinNode]]
		val kidTagStrm = ZStream.fromIterable(kidTags)

	  	val recursiveOp : UIO[(Seq[BinNode],Promise[Nothing, Option[BinNode]])] = binNodeOptPromiseMkr.flatMap(binNodeOptPromise => {
			// Under cats we could use .traverse to turn Seq[UIO] into UIO[Seq], but...
			// https://stackoverflow.com/questions/67301065/zio-transform-seqzio-to-zioseq
			val kidStrm: ZStream[Any, Nothing, BinNode] = if (maxLevels >= 2) {
				kidTagStrm.mapZIO(ktag => buildBinSubtree(meatCache, meatKeyOrder)(scenParms, maxLevels - 1)(skelNodeMap, ktag, binNodeOptPromise))
			} else ZStream.empty
			val kidsOp: ZIO[Any, Nothing, Chunk[BinNode]] = kidStrm.runCollect
			val kidSeqOp = kidsOp.map(chnk => (chnk.toSeq, binNodeOptPromise))
			kidSeqOp
		})

		val topInfoTup = topSkelNode.infoTup_opt.get
		val topBinDat = mkBinData(meatCache)(topInfoTup, scenParms)

		val topBinNodeOp: UIO[BinNode] = recursiveOp.flatMap(rrPair => {
			val (kidSubtrees, binNodeOptPromise) = rrPair
			val node = new BinNode {
				override protected def getBinData: BinData = topBinDat

				override protected def getParentOptOp: UIO[Option[BinNode]] = parentNodeOptPromise.await

				override protected def getKids: Iterable[BinNode] = kidSubtrees

				override protected def getMeatKeyOrdering: Ordering[EntryKey] = meatKeyOrder

				// FIXME: immediate toString() cannot show parent because it wants to run now.
				// Make a toString op
				override def toString: String = {
					val kidTxt = getKids.mkString("%%%")
					s"\nbuildBinSubtree.BinNode[binData=${getBinData}, kids=${kidTxt}]" // parentOpt=${getParentOptOp}
				}
			}
			val promiseFinishOp: UIO[Boolean] = binNodeOptPromise.succeed(Some(node))
			promiseFinishOp.map(_ => node)
		})
		topBinNodeOp
	}
	private def mkBinData(meatCache : MeatyItemCache)(topInfoTup : BinScalarInfoTup, scenParms: ScenarioParams): BinData = {
		val (topTimeInfo, topTagInfo, topMassInfo) = topInfoTup
		val topKeyInfo = scenParms.mkFullBinKey(topTimeInfo, topTagInfo)
		val scenID = scenParms.getScenID
		val binDat = new CacheBackedBinData {
			override def getScenarioID: BinFlavor = scenID
			override protected def getBinKey: BinFullKeyInfo = topKeyInfo
			override protected def getTimeInfo: BinTimeInfo = topTimeInfo
			override protected def getTagInfo: BinTagInfo = topTagInfo
			override protected def getMassInfo: BinMassInfo = topMassInfo

			override protected def getCache: MeatyItemCache = meatCache

			override def toString: String = s"buildBinSubtree.BinData[key=${getBinKey}, ...]"
		}
		binDat
	}
}

trait BinDataEagerLoader extends BinTreeLoader {
	def loadBinContentsEagerly(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int, maxBins : Int):
	RIO[ZDynDBExec, Chunk[(BinScalarInfoTup, BinMeatInfo)]] = {
		println(s"println.loadBinContentsEagerly BEGIN")
		val tupStrm = scalarTupStream(scenParms, maxLevels, maxBins)
		val meatyPairStrm =  joinMeatToBinScalars(meatCache)(scenParms, maxLevels)(tupStrm)
		val meatyPairChnk: ZIO[ZDynDBExec, Throwable, Chunk[(BinScalarInfoTup, BinMeatInfo)]] = meatyPairStrm.runCollect
		println(s"println.loadBinContentsEagerly END, returning chunk-ZIO: ${meatyPairChnk}")
		meatyPairChnk

		// At this point we can look at the Tags to determine how many levels we are working with.
		// We could build up a BinNode structure that can be optionally eager or lazy.

		// We can ony fetch up to 100 items in each myMeatInf-fetch (fewer if they are very big).  Batch data total < 16MB.
		// [unless there is automatic multi-batching in the ZDynDBQry.  (No reason to think there is, but COULD BE).]
		// So to "fetch any number" we would want a Stream.
		// But perhaps for now we will be content to just fetch blocks of child nodes.

	}
	private def joinMeatToBinScalars(meatCache : MeatyItemCache)(scenParms: ScenarioParams, maxLevels : Int)
						(binScInfTupStrm : ZStream[ZDynDBExec, Throwable, BinScalarInfoTup]):
						ZStream[ZDynDBExec, Throwable, (BinScalarInfoTup, BinMeatInfo)] = {
		// During this stream run, we force all myMeatInf to be loaded.
		// TODO:  Enforce maxLevels.
		println(s"println.joinMeatToBinScalars-OUTER BEGIN")
		val strmWithMeat: ZStream[ZDynDBExec, Throwable, (BinScalarInfoTup, BinMeatInfo)] = binScInfTupStrm.mapZIO(binfTup => {
			val (timeInf, tagInf, massInf) = binfTup
			val binKeyInfo = scenParms.mkFullBinKey(timeInf, tagInf)
			println(s"println.joinMeatToBinScalars-insideMapZIO made binKeyInfo to pass to cache: ${binKeyInfo}")
			val meatFetchOp : Task[Option[BinMeatInfo]] = meatCache.get(binKeyInfo)
			// By calling optMeat.get, we insist that myMeatInf-fetch must work, otherwise this stream should fail.
			val pairedWithMeatOp: Task[(BinScalarInfoTup, BinMeatInfo)] = meatFetchOp.map(optMeat => (binfTup, optMeat.get))
			// pairedWithMeatOp.tap // (pair => println("Got"))
			pairedWithMeatOp
		})
		println(s"println.joinMeatToBinScalars-OUTER END")
		strmWithMeat
	}
}