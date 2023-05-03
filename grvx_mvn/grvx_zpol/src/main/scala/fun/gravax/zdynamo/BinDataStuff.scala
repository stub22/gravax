package fun.gravax.zdynamo


import zio.{Chunk, UIO, ZIO, Random => ZRandom}
import zio.dynamodb.Item
import zio.stream.{UStream, ZSink, ZStream}

import java.math.{MathContext, RoundingMode}
import scala.collection.immutable.{Map => SMap}

private trait BinDataStuff

trait NameScopeHmm {
	// Building up data-types this way (vs. by traits) is ... extensional and sorta constructivist / algebraic.
	type EntryKey = String
	type EntryValue = BigDecimal

	type EntryMean = BigDecimal
	type EntryVar = BigDecimal	// Often this is marginal variance
	type StatEntry = (EntryKey, EntryMean, EntryVar)
	type StatRow = IndexedSeq[StatEntry]   // For some set of keySyms, in some useful order that is not specified by type.
	type StatMap = SMap[EntryKey, StatEntry]

	type DBinID = Int
	type DBinWt = BigDecimal
	type DBinDat = (DBinID, DBinWt, StatRow) // Does NOT contain covariances.

	type DBinMatrix = IndexedSeq[DBinDat]

	type WtdSqrAndMean = (BigDecimal, BigDecimal)

}
object BinTypes extends NameScopeHmm

// Generally we don't store Covariances in bins.
// Note that bins may be wide (100s of assets) and full covariance takes order-squared space.
// Instead we compute covariance for a selection of assets, on the fly.
trait BinData extends NameScopeHmm {
	def getScenarioID : String
	def getObsTime : String
	def getPredTime : String
	def getCalcTime : String
	def getBinSeqTxt : String
	def getBinSeqInt : Int
	def getParentBinSeqTxt : String
	def getParentBinSeqInt : Int
	def getBinFlavor : String // TODO: Make enum-ish
	def getRelWt : BigDecimal
	def getAbsWt : BigDecimal
	def getMass : BigDecimal

	def getStatMap : StatMap

}

case class BinTimeInfo(obsTime : String, predTime : String, calcTime : String)
case class BinSeqInfo(binSeq : String, parentBinSeq : String)
case class BinMassInfo(binMass : BigDecimal, relWt : BigDecimal, absWt : BigDecimal)

// Seems we cannot use abstract types (of our self-type, or inherited) in constructor parameters.
// If we make an outer trait scope then those names are available, or we can refer to members of an object.
case class BinMeatInfo(binFlavor : String, meatMap : BinTypes.StatMap)


case class EzBinData(scenID : String, timeDat : BinTimeInfo, seqDat : BinSeqInfo, massDat : BinMassInfo, meat : BinMeatInfo) extends BinData {
	override def getScenarioID: String = scenID
	override def getObsTime: String = timeDat.obsTime
	override def getPredTime: String = timeDat.predTime
	override def getCalcTime: String = timeDat.calcTime
	override def getBinSeqTxt: String = seqDat.binSeq
	override def getBinSeqInt: Int = ???
	override def getParentBinSeqTxt: String = seqDat.parentBinSeq
	override def getParentBinSeqInt: Int = ???
	override def getMass: BigDecimal = massDat.binMass
	override def getRelWt: BigDecimal = massDat.relWt
	override def getAbsWt: BigDecimal = massDat.absWt
	override def getBinFlavor: String = meat.binFlavor
	override def getStatMap: StatMap = meat.meatMap
}

case class BinNode(myDat : BinData, parent_opt : Option[BinNode], kids : Iterable[BinNode], meatKeyOrder : Ordering[String])  extends VecDistribFragment  {

	// Assume meatKeys are same across all bins
	override def getFullKeySymSet : Set[EntryKey] = myDat.getStatMap.keySet

	// Projects data from the main myDat layer of this BinNode, for given subset of syms.  No info from the kids is used.
	override def projectStatRow(keySyms: IndexedSeq[EntryKey]): StatRow = {
		// Will throw on failed lookup
		val meatMap = myDat.getStatMap
		val entrySeq : StatRow = keySyms.map(ksym => {
			val statAtSym: StatEntry = meatMap.get(ksym).get // This second .get will throw if no value present
			statAtSym
		}).toIndexedSeq
		entrySeq
	}

	// Useful?  This just repackages the info from myDat, with the keys in our given ordering.
	lazy val myFullBinDat : DBinDat = {
		val allSymsUnsorted = getFullKeySymSet
		val orderedSyms = allSymsUnsorted.toSeq.sorted(meatKeyOrder).toIndexedSeq
		projectToDBD(orderedSyms)
	}


	def projectToDBD (orderedSyms : IndexedSeq[EntryKey]) : DBinDat = {
		val projStatRow = projectStatRow(orderedSyms)
		(myDat.getBinSeqInt, myDat.getRelWt, projStatRow)
	}

	// TODO:  We probably want to force all children to have subtrees of equal queryDepth.
	def getMaxDepth : Int = ???

	// Collect all bins at the "queryDepth" level (not from any other levels!) into a single matrix, whose weights
	// should sum to 1.0.
	// This is the complete dataset for the depth-order (as in "nth-order") approximation to the distribution.
	// TODO: Add a stream-oriented version of this algo.
	def projectAndCollectBins(orderedSyms : IndexedSeq[EntryKey], queryDepth : Int) : DBinMatrix = {
		// TODO:  Check queryDepth <= maxDepth, else throw
		if (queryDepth == 0) {
			val onlyOneBin = projectToDBD(orderedSyms)
			IndexedSeq(onlyOneBin)
		} else {
			val childNodes = kids
			val bmtrx: Iterable[DBinDat] = childNodes.flatMap(_.projectAndCollectBins(orderedSyms, queryDepth - 1))
			bmtrx.toIndexedSeq
		}
	}
}


trait GenBinData extends KnowsBinItem {

	def mkRandMeatMap(meatKeys : Seq[String], min : BigDecimal, max : BigDecimal) // Use normal distrib?

	def dumNum : UIO[BigDecimal] = {
		val mean = BigDecimal("-5.0")
		val dev = BigDecimal("4.5")
		val mathCtx = new MathContext(8, RoundingMode.HALF_UP)
		gaussianBD(ZRandom.RandomLive, mathCtx)(mean, dev)
	}
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
	def storeBinPyramid(mmStrm : UStream[(BigDecimal, BinMeatInfo)], fixedKidsPerParent : Int, fixedNumLevels : Int) = {
		val sibChunks: UStream[Chunk[(BigDecimal, BinMeatInfo)]] = mmStrm.rechunk(fixedKidsPerParent).chunks
		// We build
		val wrkBlks = ???
		???
	}
}

/*
import java.time.{Instant => JInstant}

case class PredictionTimeInfo(obsTime : JInstant, calcTime : JInstant, endTime : JInstant)

trait BinModel {

	type QV // Quantitative value that acts like a Vector, allows computation of means.
	type ScenarioID = String

	type TimeInfo = PredictionTimeInfo

	case class BinRecord(scenario : ScenarioID, timeInf : TimeInfo, mean : QV, count_opt: Option[Int], weight_opt: Option[BigDecimal])

	def makeZDI(br : BinRecord) : Item
}

class MultiAssetBinModel extends BinModel {
	type AssetID = String

	type Amount = BigDecimal // M

	override type QV = SMap[String, Amount]

	override def makeZDI(br: BinRecord): Item = ???
}
*/
