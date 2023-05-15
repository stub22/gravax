package fun.gravax.distrib.struct

private trait BinNodeStuff


case class BinNode(myDat : BinData, parent_opt : Option[BinNode], myKids : Iterable[BinNode], meatKeyOrder : Ordering[String])  extends VecDistribFragment  {

	// Assume meatKeys are same across all bins
	// override def getFullKeySymSet : Set[EntryKey] = myDat.getStatMap.keySet

	// Projects data from the main myDat layer of this BinNode, for given subset of syms.  No info from the myKids is used.
	// Will throw on failed lookup
	override def projectShallowStatRow(keySyms: IndexedSeq[EntryKey]): StatRow = myDat.mkStatRow(keySyms)

	// Useful?  This just repackages the info from myDat, with the keys in our given ordering.
	lazy val myFullBinDat : DBinDat = {
		val keysInOrder = myDat.allKeysSorted(meatKeyOrder)
		projectToDBD(keysInOrder)
	}

	def projectToDBD (orderedSyms : IndexedSeq[EntryKey]) : DBinDat = {
		val projStatRow = projectShallowStatRow(orderedSyms)
		(myDat.getBinNumInt, myDat.getRelWt, projStatRow)
	}

	// TODO:  We often (or...always?) want to force all children to have subtrees of equal queryDepth.
	def getMaxDepth : Int = ???

	// Collect all bins at the "queryDepth" level (not from any other levels!) into a single matrix, whose weights
	// should sum to 1.0.
	// This is the complete dataset for the depth-order (as in "nth-order") approximation to the distribution.
	// TODO: Add a stream-oriented version of this algo.
	def projectAndCollectBins(orderedSyms : IndexedSeq[EntryKey], queryDepth : Int) : DBinMatrix = {
		// TODO:  Check queryDepth <= maxDepth, else throw
		if ((queryDepth == 0) || myKids.isEmpty) {
			val onlyOneBin = projectToDBD(orderedSyms)
			IndexedSeq(onlyOneBin)
		} else {
			// Recursively descend until we reach queryDepth.  Only bins from that depth will be aggregated here,
			// except when subtrees don't extend that far we will get the interim summary bins (the .isEmpty case above)
			val bmtrx: Iterable[DBinDat] = myKids.flatMap(childNode =>
				childNode.projectAndCollectBins(orderedSyms, queryDepth - 1)
			)
			bmtrx.toIndexedSeq
		}
	}
}
