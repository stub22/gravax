package fun.gravax.distrib.calc

import fun.gravax.distrib.gen.KnowsGenTypes
import fun.gravax.distrib.struct.{BinDataXformer, BinMassInfo, BinNumInfo, BinTagInfo, BinTypes}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, NonEmptyChunk, UIO}

private trait BinStatStuff

case class DBinStatClz(tagInfo: BinTagInfo, massInfo: BinMassInfo, statRow : BinTypes.StatRow)

trait BinSummaryCalc extends KnowsGenTypes  {
	val myMeatKeyOrder = Ordering.String

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
			val aggregateRowChunk: Chunk[(BinTagInfo, BinNumInfo, BinRelWt, StatRow)] = combo.map(row => {
				val (tagInfo, numInfo, (ptag, rsltChnk)) = row
				// Ensure that the parent binTags match up as expected.
				assert(tagInfo.binTag == ptag, s"tagInfo.binTag ${tagInfo.binTag} != ptag ${ptag}")
				val (aggVwt, aggStatRow) = combineWeightMeansAndVars(rsltChnk)
				val outTup = (tagInfo, numInfo, aggVwt, aggStatRow)
				outTup
			})
			aggregateRowChunk
		})
		aggUIO
	}
	def combineWeightMeansAndVars(baseRsltSeq : IndexedSeq[BinStoreRslt]) : (VagueWt, StatRow) = {
		val binStatSeq : IndexedSeq[DBinStatClz] = baseGenRsltsToDBinStats(baseRsltSeq)
		myBinStatCalcs.aggregateWeightsMeansAndVars(binStatSeq)
	}
	private def baseGenRsltsToDBinStats(baseRsltSeq : IndexedSeq[BinStoreRslt]) : IndexedSeq[DBinStatClz] = {
		val (taggedBinStats, ekeys) = baseGenRsltsToDBinStatsAndEKeys(baseRsltSeq)
		taggedBinStats
	}
	def combineVirtRsltsToWMV(virtRsltSeq : IndexedSeq[VirtRsltRow]) : (VagueWt, StatRow)  = {
		val dbdSeq : IndexedSeq[DBinStatClz] = virtRsltSeq.map(vrr => {
			val (tagInfo, numInfo, binMass, statRow) = vrr
			val massInfo = BinMassInfo(binMass, None, None)
			val dbscInst = DBinStatClz(tagInfo, massInfo, statRow)
			dbscInst
		})
		myBinStatCalcs.aggregateWeightsMeansAndVars(dbdSeq)
	}
	// type BinStoreRslt = (BinSpec, PrimaryKey, Option[Item])
	private def baseGenRsltsToDBinStatsAndEKeys(baseRsltSeq : IndexedSeq[BinStoreRslt]) :
				(IndexedSeq[DBinStatClz], IndexedSeq[EntryKey]) = {
		val binSpecs = baseRsltSeq.map(_._1)
		val firstMeat = binSpecs.head._4

		val keySeq : IndexedSeq[BinTypes.EntryKey] = firstMeat.allKeysSorted(myMeatKeyOrder)

		val binStatSeq : IndexedSeq[DBinStatClz] = binSpecs.map(binSpec => {
			val (tagInfo, numInfo, massInfo, binMeat) = binSpec
			val statRow = binMeat.mkStatRow(keySeq)
			val dbscInst = DBinStatClz(tagInfo, massInfo, statRow)
			dbscInst
		})
		(binStatSeq, keySeq)
	}

}