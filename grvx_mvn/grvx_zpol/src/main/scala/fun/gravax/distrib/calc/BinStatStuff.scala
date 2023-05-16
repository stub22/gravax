package fun.gravax.distrib.calc

import fun.gravax.distrib.gen.KnowsGenTypes
import fun.gravax.distrib.struct.{BinDataXformer, BinNumInfo, BinTagInfo, BinTypes}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, NonEmptyChunk, UIO}

private trait BinStatStuff

trait BinSummaryCalc extends KnowsGenTypes  {

	val myBinStatCalcs = new BinStatCalcs {}
	val myBDX = new BinDataXformer {}
	// type LevelTagNumChnk = NonEmptyChunk[(BinTagInfo, BinNumInfo)]

	def combineStatsPerParent(storeRsltChnk : Chunk[BinStoreRslt], parentTagNums : LevelTagNumChnk) : UIO[Chunk[VirtRsltRow]] = {

		// Results in storeRsltChnk are already sorted by parents, and should be in same order as parents in fullBinTagNumBlk
		val rsltChnksPerParent: UStream[(ParentTag, NonEmptyChunk[BinStoreRslt])] =
			ZStream.fromChunk(storeRsltChnk).groupAdjacentBy(rslt => rslt._1._1.parentTag)
		val chunkOfChunksOp: UIO[Chunk[(ParentTag, NonEmptyChunk[BinStoreRslt])]] = rsltChnksPerParent.runCollect
		val aggUIO: UIO[Chunk[VirtRsltRow]] = chunkOfChunksOp.map(cofc => {
			// If all of the parents have at least one child, then parentTagNums and cofc should be same length and in same order.
			assert(cofc.size == parentTagNums.size, s"cofc.size ${cofc.size} != parentTagNums.size ${parentTagNums.size}")
			// If inputs are NOT of equal size, the returned chunk will have the length of the shorter chunk.
			// : Chunk[(BinTagInfo, BinNumInfo, (ParentTag, NonEmptyChunk[BinStoreRslt]))]
			val combo = parentTagNums.toChunk.zip(cofc)
			assert(combo.size == parentTagNums.size, s"combo.size ${combo.size} != parentTagNums.size ${parentTagNums.size}")
			// Could make this a stream to
			// combo.foreach(row =>
			val aggregateRowChunk: Chunk[(BinTagInfo, BinNumInfo, DBinWt, StatRow)] = combo.map(row => {
				val (tagInfo, numInfo, (ptag, rsltChnk)) = row
				// Ensure that the parent binTags match up as expected.
				assert(tagInfo.binTag == ptag, s"tagInfo.binTag ${tagInfo.binTag} != ptag ${ptag}")
				val (aggWt, aggStatRow) = combineWeightMeansAndVars(rsltChnk)
				val outTup = (tagInfo, numInfo, aggWt, aggStatRow)
				outTup
			})
			aggregateRowChunk
		})
		aggUIO
	}
	def combineWeightMeansAndVars(baseRsltSeq : IndexedSeq[BinStoreRslt]) : (DBinWt, StatRow) = {
		val binDatChunk : IndexedSeq[DBinDat] = baseGenRsltsToDBinDats(baseRsltSeq)
		myBinStatCalcs.aggregateWeightsMeansAndVars(binDatChunk)
	}
	private def baseGenRsltsToDBinDats(baseRsltSeq : IndexedSeq[BinStoreRslt]) : IndexedSeq[DBinDat] = {
		val (taggedDBDs, ekeys) = baseGenRsltsToTaggedDBinDatsAndEKeys(baseRsltSeq)
		taggedDBDs.map(_._3)
	}
	def combineVirtRsltsToWMV(virtRsltSeq : IndexedSeq[VirtRsltRow]) : (DBinWt, StatRow)  = {
		val dbdSeq : IndexedSeq[DBinDat] = virtRsltSeq.map(vrr => {
			val (tagInfo, numInfo, binWt, statRow) = vrr
			val binIdHmm = -999 // tagInfo.binTag
			val dbd = (binIdHmm, binWt, statRow )
			dbd
		})
		myBinStatCalcs.aggregateWeightsMeansAndVars(dbdSeq)
	}
	// type BinStoreRslt = (BinSpec, PrimaryKey, Option[Item])
	private def baseGenRsltsToTaggedDBinDatsAndEKeys(baseRsltSeq : IndexedSeq[BinStoreRslt]) :
				(IndexedSeq[(ParentTag, BinTag, DBinDat)], IndexedSeq[EntryKey]) = {
		val binSpecs = baseRsltSeq.map(_._1)
		val firstMeat = binSpecs.head._4
		val meatKeyOrder = Ordering.String
		val keySeq : IndexedSeq[BinTypes.EntryKey] = firstMeat.allKeysSorted(meatKeyOrder)
		val binDatSeq : IndexedSeq[(ParentTag, BinTag, DBinDat)] = binSpecs.map(binSpec => {
			val dbd : DBinDat = myBDX.binSpecToDBD(binSpec, keySeq)
			val tagInfo : BinTagInfo = binSpec._1
			(tagInfo.parentTag, tagInfo.binTag, dbd)
		})
		(binDatSeq, keySeq)
	}

}