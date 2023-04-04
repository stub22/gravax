package fun.gravax.xtyp.histo

import scala.collection.immutable.TreeMap

private trait DiscreteHistoStuff

case class DiscreteHistoBin[BinKey](binKey : BinKey, count : Int) {
	def incr: DiscreteHistoBin[BinKey] = {
		val oldCnt = count
		copy(count = oldCnt + 1)
	}

	def combineWithMatchedBin(otherBin : DiscreteHistoBin[BinKey]) : DiscreteHistoBin[BinKey] = {
		assert(otherBin.binKey == binKey)
		val sum = count + otherBin.count
		copy(count = sum)
	}


}
// TreeMap uses an implicit Ordering[BinKey].
// This histo *can* automatically expand to contain bins for new keys.
case class DiscreteHisto[BinKey : Ordering](binMap : TreeMap[BinKey, DiscreteHistoBin[BinKey]]) {
	private def binFor(bk : BinKey) = {
		binMap.get(bk)
	}
	def addSample(bk : BinKey) : DiscreteHisto[BinKey] = {
		val nextMap = binMap.updatedWith(bk)(prevBinOpt => Some(prevBinOpt.fold(
			new DiscreteHistoBin[BinKey](bk, 1)) // None case
		(prevBin => prevBin.incr))) // Some case
		copy(binMap = nextMap)
	}

	def combine(otherH : DiscreteHisto[BinKey]) : DiscreteHisto[BinKey] = {
		// TODO:  Do this in a more monoid-combiney way.
		// https://stackoverflow.com/questions/20047080/scala-merge-map
		val myPairs = binMap.toSeq
		val otherPairs = otherH.binMap.toSeq
		val combinedPairs: Seq[(BinKey, DiscreteHistoBin[BinKey])] = myPairs ++ otherPairs
		val summed: Map[BinKey, DiscreteHistoBin[BinKey]] = combinedPairs.groupMapReduce(_._1)(_._2)(_.combineWithMatchedBin(_))
		val comboTreeMap  = TreeMap.from[BinKey, DiscreteHistoBin[BinKey]](summed) // uses implicit Ordering
		DiscreteHisto(comboTreeMap)
	}
	def totalSampleCount : Int = {
		binMap.foldLeft[Int](0)((prevSum, nxtPair) => {
			prevSum + nxtPair._2.count
		})
	}
}