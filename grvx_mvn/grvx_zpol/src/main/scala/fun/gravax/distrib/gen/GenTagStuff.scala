package fun.gravax.distrib.gen

import fun.gravax.distrib.struct.{BinNumInfo, BinTagInfo}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, NonEmptyChunk, UIO}

import scala.collection.immutable.Queue

private trait GenTagStuff


trait GoodTagNumBlk extends KnowsGenTypes {
	def describe : String
	def getBaseLevel : LevelTagNumChnk
	def getVirtLevelsChnk : Chunk[(Int, LevelTagNumChnk)]

}


trait GenTagNumData extends KnowsGenTypes {
	// BinTagNumBlock is reified as a case class because this is a useful boundary from design perspective.
	// We separate the baseLevel because it is semantically distinct, it is used for independent bin data.
	// The virtLevels are for derived data.  However these levels are all represented using the same tag-key structures,
	// so we COULD instead treat all the levels in this BinTagNumBlock as "the same".  In our generative use case,
	// they are all snapped out of a common stream of nums.  If we were accepting "real" base-bin info, we probably
	// would get the base-bin data standalone, (perhaps then do some clustering on it), and then build the virtLevels
	// tree structure in a more incremental fashion.  We want all this code to care as little as possible which
	// of those scenarios we are in.

	case class BinTagNumBlock(baseLevel : LevelTagNumChnk, virtLevels : Chunk[(Int, LevelTagNumChnk)]) extends GoodTagNumBlk {
		override def describe : String = {
			val virtLevelsSizes = virtLevels.map(levPair => (levPair._1, levPair._2.size))
			s"BinTagNumBlock baseLevel.count=${baseLevel.size}, virtLevels.count=${virtLevels.size}, innerSizes=${virtLevelsSizes}"
		}

		override def getBaseLevel: LevelTagNumChnk = baseLevel

		override def getVirtLevelsChnk: Chunk[(Int, LevelTagNumChnk)] = virtLevels

	}


	// Our resulting op is currently pure and deterministic (no Random nums).
	// But if someone changed the impl of genTagNumChunksForAllLevels...
	def genBinTagNumBlock(fixedFlavor : BinFlavor)(rootSeqNum : Int,  rootKidCnt : Int, baseLevel : Int): UIO[BinTagNumBlock] = {
		val taggyLevelChunkOp : UIO[Chunk[(Int, LevelTagNumChnk)]] = genTagNumChunksForAllLevels(fixedFlavor)(rootSeqNum, rootKidCnt, baseLevel)

		val levelTuplesOp = taggyLevelChunkOp.map(outerChnk => {
			// We know based on how seqInfoStrm works that these inner pairs should happen to be in level-number order.
			// (Would like that to be conveyed by the type:  Sorted)
			// However, for more generality we avoid that assumption here.
			val ochnk: Chunk[(Int, NonEmptyChunk[(BinTagInfo, BinNumInfo)])] = outerChnk
			val frontPairs: Chunk[(Int, NonEmptyChunk[(BinTagInfo, BinNumInfo)])] = ochnk.filter(kvEnt => kvEnt._1 < baseLevel).sortBy(kvEnt => kvEnt._1)
			val baseChunk: NonEmptyChunk[(BinTagInfo, BinNumInfo)] = ochnk.filter(kvEnt => kvEnt._1 == baseLevel).head._2
			(baseChunk, frontPairs)
		})
		val bntgnmBlkOp: UIO[BinTagNumBlock] = levelTuplesOp.map(tuple => BinTagNumBlock(tuple._1, tuple._2))
		bntgnmBlkOp
	}
	// This job should be pure and deterministic, always producing the same (potentially large) result.
	// (assuming that genTagInfoStrm is pure+deterministic).
	// We need to capture that whole result as a single output step to ensure consistency of all the parent-tag-links
	// between bins.
	def genTagNumChunksForAllLevels(fixedFlavor : BinFlavor)(rootSeqNum : Int,  rootKidCnt : Int, maxLevels : Int): UIO[Chunk[(Int, LevelTagNumChnk)]] = {
		val seqInfoStrm: UStream[(BinTagInfo, BinNumInfo)] = genTagInfoStrm(fixedFlavor)(rootSeqNum, rootKidCnt)
		val chnksByLevStrm: UStream[(Int, LevelTagNumChnk)] = seqInfoStrm.groupAdjacentBy(infoPair => infoPair._2.levelNum)
		// runCollect collects ALL items from the input, so we truncate the input first.
		chnksByLevStrm.take(maxLevels).runCollect
	}
	// These params combine with rootKidsCnt to determine the shape of the bin pyramid.
	val myGrandKidShrinkage = 1
	val myReproShrinkPerLevel = 3 // subtracted from prev level for ggkids and subsequent

	// TODO:  Add independent calc of the size of the tagInfoStream (for finite cases) and assert equality
	// between that calc-size and the measured size of this stream.
	def calcTotalBinsIfFinite(rootKidsCnt : Int) : Option[Int] = ???

	// This numbering stream contains all levels of our expected structure.  It *could* be nondeterministic,
	// in which case we can't rely on any ability to run it multiple times (which would give different answers)
	// for the same scenario.  Rather we must capture all the useful/important parts of stream on the first(/only) run.
	// Further streaming operations must then be built using those captured results.

	// Breadth-first traversal approach using a queue will work, at a reasonable memory cost.
	// This impl is pure and deterministic - no random numbers here.  Result is always the same.
	def genTagInfoStrm(fixedFlav : BinFlavor)(rootSeqNum : Int, rootKidsCnt : Int) : UStream[(BinTagInfo, BinNumInfo)] = {
		// We want the largest-granularity entries to appear first
		// root, child_01...child_N, grandchild_01-01...01-N...N-01..N-N, ggchild_01-01-01...
		// state = (absIndex, Vector(posInLevel_1, posInLevel_2, ...))
		// starting abs-position for each level is sum of the lengths of the higher levels.
		// posWithinSiblings

		case class ParentRec(seqNum : Int, numKids : Int, parLevelNum : Int)
		// val initPosVec = Vector[Int](1)
		val emptyParentQ = Queue.empty[ParentRec]
		val initParentQ = emptyParentQ.enqueue(ParentRec(rootSeqNum, rootKidsCnt, 1))

		case class GenSt(parent_opt : Option[ParentRec], absIdx : Int, locIdx : Int, levelNum : Int, maxKids : Int) //  parentQ : Queue[ParentRec] )
		// Root is special and must have

		val initGenSt = GenSt(None, rootSeqNum, 1, 1, rootKidsCnt)
		val initUnfoldState = (initGenSt, initParentQ)
		val noGenSt = GenSt(None, -100, -200, -300, -400)
		// The size of each kid-block MAY be chosen dynamically in-stream.
		// Setting this value here to illustrate computation BEFORE we enter stream context, which is just one way
		val grandkidsPerRootkidCnt = rootKidsCnt - myGrandKidShrinkage

		// ZStream.iterate is necessarily infinite.  So we are using .unfold, which returns Option[Result, NxtSt].
		// But note that unFold does not emit the INITIAL state,
		val genStStrm: UStream[GenSt] = ZStream.unfold[(GenSt, Queue[ParentRec]), GenSt](initUnfoldState)(prevUnfSt => {
			val (prevGenSt, prevParQ) = prevUnfSt
			val prevLocIdx = prevGenSt.locIdx
			val prevLevNum = prevGenSt.levelNum
			val prevMaxKids = prevGenSt.maxKids
			val nextAbsIdx = prevGenSt.absIdx + 1

			// FIXME: There are too many cases here
			val (nextGenSt, midParQ) = prevGenSt.parent_opt match {
				case None => {
					// Special
					val prevParOpt = prevParQ.dequeueOption
					prevParOpt match {
						case None => (noGenSt, emptyParentQ)
						case Some((nextPRec, middleParentQ)) => {
							val nextGenSt = GenSt(Some(nextPRec), nextAbsIdx, 1, 2, grandkidsPerRootkidCnt)
							(nextGenSt, middleParentQ)
						}
					}
				}
				case Some(prevParRec) => {
					// prevParRec is the parent of the last bin emitted.  It should be same as LAST/NEWEST record in prevParQ
					if (prevLocIdx >= prevParRec.numKids) {
						// We are ending the prev sequence of siblings, selecting new parent, which may change level.
						// This record will be the first kid of its parent, and first among a new set of siblings,
						// who all hit the else-clause below, and there copy our values for levNum and maxKid.
						val prevParOpt = prevParQ.dequeueOption // Get the next parent
						prevParOpt match {
							case None => (noGenSt, emptyParentQ)
							case Some((nextPRec, mParQ)) => {
								val nxtParNumKids = nextPRec.numKids
								if (nxtParNumKids > 0) {
									val nxtLevNum = nextPRec.parLevelNum + 1
									val nxtMaxKids = Math.max(nxtParNumKids - myReproShrinkPerLevel, 0)
									// Here we may CHOOSE our own val for maxKids, and it will propagate thru our siblings.
									val nextGenSt = GenSt(Some(nextPRec), nextAbsIdx, 1, nxtLevNum, nxtMaxKids)
									(nextGenSt, mParQ)
								} else {
									// Parent says no getKids so nothing to do here.
									(noGenSt, mParQ)
								}
							}
						}
					} else {
						// 2nd and following siblings hit this clause.  We maintain status quo: same parent, etc.
						// Currently we choose to continue with prevMaxKids, but we could vary it here.
						(GenSt(Some(prevParRec), nextAbsIdx, prevLocIdx + 1, prevLevNum, prevMaxKids), prevParQ)
					}
				}
			}
			val chooseNumKidsHere = nextGenSt.maxKids // we could choose a value less than max here
			val upParQ = if(chooseNumKidsHere > 0) {
				val ourParRec = ParentRec(nextGenSt.absIdx, chooseNumKidsHere, nextGenSt.levelNum)
				midParQ.enqueue(ourParRec)
			} else midParQ
			val recToEmit = nextGenSt
			if (nextGenSt == noGenSt) None else Some((recToEmit, (nextGenSt, upParQ)))
		})
		// The above does not emit the first ROOT record/state, so we prepend a one-element stream here.
		val firstGenStStrm = ZStream(initGenSt)
		val fullGenStStrm = firstGenStStrm concat genStStrm
		fullGenStStrm.map(genSt => {
			val parentNum = genSt.parent_opt.fold(-1)(prec => prec.seqNum)
			val levelNum = genSt.levelNum
			val maxKids = genSt.maxKids
			val numInfo = BinNumInfo(genSt.absIdx, parentNum, maxKids, levelNum, genSt.locIdx)
			val chldTxt = genSt.absIdx.toString
			val parTxt : String = genSt.parent_opt.fold(PTAG_NO_PARENT)(prec => prec.seqNum.toString)
			val tagInfo = BinTagInfo(chldTxt, parTxt, fixedFlav)
			(tagInfo, numInfo)
		})
	}
	/* Using an increment-digits-and-carry approach, can do a single pass to generate the levels numbers,
	but would also need to weave in the parent-number memory.  Could make the posVec entries into tuples,
	that know (parentNum, ourPosInParentsSiblings)
	val x = ZStream.iterate(initState)(prevSt => {
		val (prevParent, prevAbsIdx, prevPosVec)  = prevSt
		val prevDepth = prevPosVec.length
		val prevLast = prevPosVec.last
		if (prevDepth > 1) {
			val nextPosVec = if (prevLast == rootKidsCnt) {
				val unfinishedDigits = prevPosVec.reverse.dropWhile(digit => digit == rootKidsCnt).reverse
				val incompLen = unfinishedDigits.length
				val nextDepth = if (incompLen == 1) prevDepth + 1 else prevDepth
				// val moreDigits = Vector.fill[Int](nextDepth - incompLen)(1)

				val lastUnfDig = unfinishedDigits.last // We know it is < rootKidsCnt
				val incrLead = unfinishedDigits.updated(incompLen - 1, lastUnfDig + 1)
				val nextVec = incrLead.padTo(nextDepth, 1)
				nextVec
			} else prevPosVec.updated(prevDepth - 1, prevLast + 1)
			()
		} else {
			val posVec = Vector[Int](1, 1)
			(1, 1, posVec)
		}
	})
	 */


}