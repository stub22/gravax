package fun.gravax.zdynamo


import zio.{Chunk, UIO, ZIO, Random => ZRandom}
import zio.stream.{UStream, ZStream}

import java.math.{MathContext, RoundingMode}
import scala.collection.immutable.{Queue, Map => SMap}

private trait GenBinStuff

trait GenBinData extends KnowsBinItem {

	val myTBI : ToBinItem

	def gaussianBD(zrnd : ZRandom, mathCtx : MathContext)(mean : BigDecimal, stdDev : BigDecimal): UIO[BigDecimal] = {
		val stdNumEff: UIO[Double] = zrnd.nextGaussian
		stdNumEff.map(num => {
			// https://blogs.oracle.com/javamagazine/post/four-common-pitfalls-of-the-bigdecimal-class-and-how-to-avoid-them
			val stdBD = BigDecimal(num, mathCtx)
			val scaledAndShiftedBD = stdBD.*(stdDev).+(mean)
			scaledAndShiftedBD
		})
	}

	def truncGaussBD(zrnd : ZRandom, mathCtx : MathContext)(min : BigDecimal, max : BigDecimal): UIO[BigDecimal] = {
		val diff = max.-(min)
		val mean = min.+(diff./(2))
		val stdDev = diff./(3)
		val gbdEff = gaussianBD(zrnd, mathCtx)(mean, stdDev)
		val bgbdEff : UIO[BigDecimal]  = gbdEff.repeatWhile(rslt => (rslt.compareTo(min) < 0 || rslt.compareTo(max) > 0))
		bgbdEff
	}

	val (meanMin, meanMax) = (BigDecimal("-1.0"), BigDecimal("1.0"))
	val (vrMin, vrMax) = (BigDecimal("0.0"), BigDecimal("0.5"))
	def genRandStatEntry(zrnd : ZRandom, mathCtx : MathContext)(ekey : BinTypes.EntryKey) : UIO[BinTypes.StatEntry] = {
		for {
			mean <- truncGaussBD(zrnd, mathCtx)(meanMin, meanMax)
			vr <- truncGaussBD(zrnd, mathCtx)(vrMin, vrMax)
		} yield (ekey, mean, vr)
	}


	def genRandStatMap(zrnd : ZRandom, mathCtx : MathContext)(keys : Iterable[BinTypes.EntryKey]) : UIO[BinTypes.StatMap] = {
		val kstrm: UStream[BinTypes.EntryKey] = ZStream.fromIterable(keys)
		// This could be done in fewer lines using ZSink.collectAllToMap
		val stStrm: UStream[(BinTypes.EntryKey, BinTypes.StatEntry)] = kstrm.flatMap(key => {
			val steOp  = genRandStatEntry(zrnd, mathCtx)(key)
			val tupOp = steOp.map(statEnt => (statEnt._1, statEnt))
			ZStream.fromZIO(tupOp)
		})
		// val chunkMkrSink = ZSink.collectAll[(BinTypes.EntryKey, BinTypes.StatEntry)]
		val stChunk_op: UIO[Chunk[(BinTypes.EntryKey, BinTypes.StatEntry)]] = stStrm.runCollect
		val stMap: UIO[SMap[BinTypes.EntryKey, BinTypes.StatEntry]] = stChunk_op.map(chnk => chnk.toMap)
		stMap
	}
	def genRandEntryKey(zrnd : ZRandom, keyLen : Int) : UIO[BinTypes.EntryKey] = {
		val (minKeyChr, maxKeyChr) = ('A', 'Z')
		val oneChrOp: UIO[Char] = zrnd.nextIntBetween(minKeyChr, maxKeyChr + 1).map(_.toChar)
		val manyChrStrm = ZStream.repeatZIO(oneChrOp)
		val enoughChrStrm = manyChrStrm.take(keyLen)
		// val acs = ZSink.collectAll[Char]
		val echk: UIO[Chunk[Char]] = enoughChrStrm.runCollect
		val esop: UIO[String] = echk.map(chnk => new String(chnk.toArray))
		esop
	}
	def genManyEKeys (zrnd : ZRandom, keyLen : Int, numKeys : Int) : UIO[Seq[BinTypes.EntryKey]] = {
		val randEkeyOp = genRandEntryKey(zrnd, keyLen)
		val keyStrm = ZStream.repeatZIO(randEkeyOp)
		val enoughKeysOp = keyStrm.take(numKeys).runCollect.map(_.toSeq)
		enoughKeysOp
	}
	// Generate a stream of random stat maps, and treat each as the meat-map of a bin.
	def genMeatInfoStrmFromFixedKeys(zrnd : ZRandom, mathCtx : MathContext, fixedKeys : Seq[BinTypes.EntryKey], fixedFlavor : String): UStream[BinMeatInfo] = {
		val oneStMpOp = genRandStatMap(zrnd, mathCtx)(fixedKeys)
		val stMpStrm = ZStream.repeatZIO(oneStMpOp)
		val miStrm = stMpStrm.map(stMap => BinMeatInfo(fixedFlavor, stMap))
		miStrm
	}

	val (massMin, massMax) = (BigDecimal("1.0"), BigDecimal("1000.0"))
	val (myKeyLen, myNumKeys) = (3, 50)

	def mkMassGenOp(zrnd: ZRandom, mathCtx: MathContext): UIO[BigDecimal] = truncGaussBD(zrnd, mathCtx)(massMin, massMax)
	def addMassToMeatStrm(meatStrm : UStream[BinMeatInfo], massGenOp : UIO[BigDecimal]) : UStream[(BigDecimal, BinMeatInfo)] = {
		meatStrm.mapZIO(bmi => massGenOp.map(massBD => (massBD, bmi)))
	}

	// The configuration of this stream is sprinkled in the vals, currently Stu counts 8 of em
	// Each stream tuple is ready to be the genesis of a leaf-bin (with no kiddos)
	def mkMassyMeatStrm(zrnd: ZRandom, mathCtx: MathContext) : UStream[(BigDecimal, BinMeatInfo)] = {

		val binFlavor = BFLV_ANN_RET_MEAN_VAR
		val ekeysOp : UIO[Seq[BinTypes.EntryKey]] = genManyEKeys(zrnd, myKeyLen, myNumKeys)
		val ekeyStrmOfSeq = ZStream.fromZIO(ekeysOp)
		val massyMeatStrm : UStream[(BigDecimal, BinMeatInfo)] = ekeyStrmOfSeq.flatMap(keySeq => {
			val meatInfoStrm = genMeatInfoStrmFromFixedKeys(zrnd, mathCtx, keySeq, binFlavor)
			val massOp = mkMassGenOp(zrnd, mathCtx)
			val massyMeatStrm = addMassToMeatStrm(meatInfoStrm, massOp)
			massyMeatStrm
		})
		massyMeatStrm
	}

	// Absolute-weight field is the sticking point.  We need to know the total mass (of the distribution, == sum of all leaf bins)
	// before we write the first bin to DB.  Otherwise we have to make a second pass to store absWeight.
	// Believe we have not yet written any app code that uses absolute weight.
	// 2023-05-03 :  AbsoluteWeight is disabled until further notice.
	def storeBinPyramid(scenID : String, timeInf : BinTimeInfo)(mmStrm : UStream[(BigDecimal, BinMeatInfo)], fixedKidsPerParent : Int, fixedNumLevels : Int) = {
		val skelBintem = myTBI.mkBinItemSkel(scenID, timeInf)

		val rootSeqNum = 400
		val seqInfoStrm = genTagInfoStrm(rootSeqNum, fixedKidsPerParent)
	//	val binIdxPairStrm
		// Top level always has one bin
		val kppBD = BigDecimal(fixedKidsPerParent)
		val bottomLevelCnt = kppBD.pow(fixedNumLevels - 1)
		val sibChunks: UStream[Chunk[(BigDecimal, BinMeatInfo)]] = mmStrm.rechunk(fixedKidsPerParent).chunks
//		val chnksWithTotalMass = sibChunks.map(chnk => chnk.foldLeft())
		// We build
		// val wrkBlks = sibChunks.
		???
	}

	// def nextDigitVec(prevDigitVec : Vector[Int], minDigitIncl : Int, maxDigitIncl : Int) : Vector[Int] = {

	// Breadth-first traversal approach using a queue will work, at a reasonable memory cost
	// def childrenOfPrefix(parentID, digitPrefix, startId, numKids) :
	// TODO: make number of kids come from a stream, with one entry per LEVEL.  Ooh this is kinda hard to do in one pass.
	def genTagInfoStrm(rootSeqNum : Int, rootKidsCnt : Int) : UStream[(BinTagInfo, BinNumInfo)] = {
		// We want the largest-granularity entries to appear first
		// root, child_01...child_N, grandchild_01-01...01-N...N-01..N-N, ggchild_01-01-01...
		// state = (absIndex, Vector(posInLevel_1, posInLevel_2, ...))
		// starting abs-position for each level is sum of the lengths of the higher levels.
		// posWithinSiblings

		case class ParentRec(seqNum : Int, numKids : Int, parLevelNum : Int)
		// val initPosVec = Vector[Int](1)
		val emptyParentQ = Queue.empty[ParentRec]
		val initParentQ = emptyParentQ.enqueue(ParentRec(1, rootKidsCnt, 1))

		case class GenSt(parent_opt : Option[ParentRec], absIdx : Int, locIdx : Int, levelNum : Int, maxKids : Int) //  parentQ : Queue[ParentRec] )
		// Root is special and must have

		val initState = (GenSt(None, rootSeqNum, 1, 1, rootKidsCnt), initParentQ)
		val noGenSt = GenSt(None, -100, -200, -300, -400)
		// The size of each kid-block MAY be chosen dynamically in-stream.
		// Setting this value here to illustrate computation BEFORE we enter stream context, which is just one way
		val grandkidsPerRootkidCnt = rootKidsCnt - 1
		// ZStream.iterate is necessarily infinite.
		// If we want to allow for
		val pairStrm: UStream[(GenSt, Queue[ParentRec])] = ZStream.iterate[(GenSt, Queue[ParentRec])](initState)(prevSt => {
			val (prevGenSt, prevParQ) = prevSt
			val prevLocIdx = prevGenSt.locIdx
			val prevLevNum = prevGenSt.levelNum
			val prevMaxKids = prevGenSt.maxKids
			val nextAbsIdx = prevGenSt.absIdx + 1

			val (nextGenSt, midParQ) = prevGenSt.parent_opt match {
				case None => {
					val (nextPRec, middleParentQ) = prevParQ.dequeue
					val nextGenSt = GenSt(Some(nextPRec), nextAbsIdx, 1, 2, grandkidsPerRootkidCnt)
					(nextGenSt, middleParentQ)
				}
				case Some(prevParRec) => {
					// prevParRec is the parent of the last bin emitted.  It should be same as LAST/NEWEST record in prevParQ
					if (prevLocIdx >= prevParRec.numKids) {
						// We are ending the prev sequence of siblings, selecting new parent, which may change level.
						// This record will be the first kid of its parent, and first among a new set of siblings,
						// who all hit the else-clause below, and there copy our values for levNum and maxKid.
						val prevParOpt = prevParQ.dequeueOption // Get the next parent
						prevParOpt match {
							case Some((nextPRec, mParQ)) => {
								val nxtParNumKids = nextPRec.numKids
								if (nxtParNumKids > 0) {
									val nxtLevNum = nextPRec.parLevelNum + 1
									val nxtMaxKids = Math.max(nxtParNumKids - 3, 0)
									// Here we may CHOOSE our own val for maxKids, and it will propagate thru our siblings.
									val nextGenSt = GenSt(Some(nextPRec), nextAbsIdx, 1, nxtLevNum, nxtMaxKids)
									(nextGenSt, mParQ)
								} else {
									// Parent says no kids so nothing to do here.
									(noGenSt, mParQ)
								}
							}
							case None => (noGenSt, emptyParentQ)
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
			(nextGenSt, upParQ)
		})
		pairStrm.map(gqPair => {
			val genSt = gqPair._1
			val parentNum = genSt.parent_opt.fold(-1)(prec => prec.seqNum)
			val levelNum = genSt.levelNum
			val maxKids = genSt.maxKids
			val numInfo = BinNumInfo(genSt.absIdx, parentNum, maxKids, levelNum, genSt.locIdx)
			val chldTxt = genSt.absIdx.toString
			val parTxt = genSt.parent_opt.fold("NO_PARENT")(prec => prec.seqNum.toString)
			val tagInfo = BinTagInfo(chldTxt, parTxt)
			(tagInfo, numInfo)
		})
		/* Using an increment-digits-and-carry approach, can do a single pass to generate the levels numbers,
		but would also need to weave in the parent-number memory.  Could make the posVec entries into tuples,
		that know (parentNum, ourPosInParentsSiblings)
		val x = ZStream.iterate(initState)(prevSt => {
			val (prevParent, prevAbsIdx, prevPosVec)  = prevSt
			val prevDepth = prevPosVec.length
			val prevLast = prevPosVec.last
			if (prevDepth > 1) {
				// This is easier if we use mutab
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

	/*

		val fleshyBI = myTBI.fleshOutBinItem(skelBinItem, binSeqNum, parentBinSeqNum, binMass, binRelWeight) // , binAbsWeight, )

		val meatyBI = myTBI.addMeatToBinItem(fleshyBI, meatInfo)
		val fullBI = myTBI.fillBinSortKey(meatyBI)
		val ourPK: PrimaryKey = myFBI.getPKfromFullBinItem(fullBI)
		putAndLog(binTblNm, fullBI).map(_ => ourPK)
	 */
}