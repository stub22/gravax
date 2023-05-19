package fun.gravax.distrib.struct

import zio.{Task, ZIO}
import zio.stream.{UStream, ZStream}

private trait BinNodeStuff


trait BinNode extends VecDistribFragment  {
	protected def getBinData : BinData
	protected def getParent_opt : Option[BinNode]
	protected def getKids : Iterable[BinNode]
	protected def getMeatKeyOrdering : Ordering[EntryKey]

	// Assume meatKeys are same across all bins
	// override def getFullKeySymSet : Set[EntryKey] = getBinData.getStatMap.keySet

	// Projects data from the main getBinData layer of this BinNode, for given subset of syms.  No info from the getKids is used.
	// Will throw on failed lookup
	override def projectShallowStatRow(keySyms: IndexedSeq[EntryKey]): Task[StatRow] = getBinData.mkStatRow(keySyms)

	// Useful?  This just repackages the info from getBinData, with the keys in our given ordering.
	/* private lazy val myFullBinDat : DBinDat = {
		val keysInOrder = getBinData.allKeysSorted(getMeatKeyOrdering)
		projectToDBD_op(keysInOrder)
	}*/

	def projectToDBD_op(orderedSyms : IndexedSeq[EntryKey]) : Task[DBinDat] = {
		val projStatRowOp = projectShallowStatRow(orderedSyms)
		projStatRowOp.map(psrow => (getBinData.getBinNumInt, getBinData.getRelWt, psrow))
	}

	def allKeysSorted : Task[Seq[EntryKey]] = {
		val meatKeyOrder = getMeatKeyOrdering
		getBinData.allKeysSorted(meatKeyOrder)
	}

	// TODO:  We often (or...always?) want to force all children to have subtrees of equal queryDepth.
	def getMaxDepth : Int = ???

	// Collect all bins at the "queryDepth" level (not from any other levels!) into a single matrix, whose weights
	// should sum to 1.0.
	// This is the complete dataset for the depth-order (as in "nth-order") approximation to the distribution.
	// TODO: Add a stream-oriented version of this algo.
	def projectAndCollectBins(orderedSyms : IndexedSeq[EntryKey], queryDepth : Int) : Task[DBinMatrix] = {
		// TODO:  Check queryDepth <= maxDepth, else throw
		if ((queryDepth == 0) || getKids.isEmpty) {
			projectToDBD_op(orderedSyms).map(oneBin => IndexedSeq(oneBin))
		} else {
			// Recursively descend until we reach queryDepth.  Only bins from that depth will be aggregated here,
			// except when subtrees don't extend that far we will get the interim summary bins (the .isEmpty case above)
			val kidStream: UStream[BinNode] = ZStream.fromIterable(getKids)
			val kidBinStream: ZStream[Any, Throwable, DBinDat] = kidStream.flatMap(childNode => {
				val childMatrixOp = childNode.projectAndCollectBins(orderedSyms, queryDepth - 1)
				val streamOfOneMatrix = ZStream.fromZIO(childMatrixOp)
				val streamOfDBDs: ZStream[Any, Throwable, DBinDat] = streamOfOneMatrix.flatMap(matrix => ZStream.fromIterable(matrix))
				streamOfDBDs
			})
			val out = kidBinStream.runCollect //  .map(chnk => chnk)
			out
				// ZStream.fromZIO())
			// val fs = kidBinStream.flatten
		//	val bmtrx: Iterable[DBinDat] = getKids.flatMap(childNode =>
		//		childNode.projectAndCollectBins(orderedSyms, queryDepth - 1)
		//	)
		//	bmtrx.toIndexedSeq
		}
	}
}
