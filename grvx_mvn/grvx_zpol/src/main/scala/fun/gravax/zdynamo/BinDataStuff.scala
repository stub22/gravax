package fun.gravax.zdynamo

import zio.{UIO, Random => ZRandom}
import zio.dynamodb.Item

import java.math.{MathContext, RoundingMode}
import scala.collection.immutable.{Map => SMap}

private trait BinDataStuff

trait BinData {
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
	def getAnnRetMeans : Map[String, BigDecimal]
}
case class BinTimeData(obsTime : String, predTime : String, calcTime : String)
case class BinSeqInfo(binSeq : String, parentBinSeq : String)
case class BinMassInfo(binMass : BigDecimal, relWt : BigDecimal, absWt : BigDecimal)
case class BinMeat(binFlavor : String, meatMap : SMap[String, BigDecimal])
case class EzBinData(scenID : String, timeDat : BinTimeData, seqDat : BinSeqInfo, massDat : BinMassInfo, meat : BinMeat) extends BinData {
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
	override def getAnnRetMeans: SMap[String, BigDecimal] = meat.meatMap


}
trait NameScopeHmm {
	type KeySym = String
	type RowEntry = (KeySym, BigDecimal)
	type Row = Seq[RowEntry]   // For some set of keySyms, in some useful order that is not specified by type.

	type DBinID = Int
	type DBinWt = BigDecimal
	type DBinDat = (DBinID, DBinWt, Row)
}
trait VecDistribFragment extends NameScopeHmm {


	// We expect Assets (meatKeys) to be identical across all bins
	def	getFullKeySymSet : Set[KeySym] // The syms do not have a canonical ordering.  Client may use alphabetic, or...
	def projectMean(keySyms : Seq[KeySym]) : Row // Often this is available directly from VecDistrib-bin-0

}
class BinNode(myDat : BinData, parent_opt : Option[BinNode], kids : Iterable[BinNode], meatKeyOrder : Ordering[String])  extends VecDistribFragment  {

	// Assume meatKeys are same across all bins
	override def getFullKeySymSet : Set[KeySym] = myDat.getAnnRetMeans.keySet


	override def projectMean(keySyms: Seq[KeySym]): Row = {
		// Will throw on failed lookup
		val meatMap = myDat.getAnnRetMeans
		val entrySeq : Row = keySyms.map(ksym => {
			val numValAtSym : BigDecimal = meatMap.get(ksym).get // This second .get will throw if no value present
			(ksym, numValAtSym)
		})
		entrySeq
	}

	lazy val myFullBinDat : DBinDat = {
		val allSymsUnsorted = getFullKeySymSet
		val orderedSyms = allSymsUnsorted.toSeq.sorted(meatKeyOrder)
		projectToDBD(orderedSyms)
	}

	def projectToDBD (orderedSyms : Seq[KeySym]) : DBinDat = {
		val meanDat = projectMean(orderedSyms)
		(myDat.getBinSeqInt, myDat.getRelWt, meanDat)
	}
}

trait VecDistrib extends VecDistribFragment {

	// TODO:  Refine to a triangular rep.
	type CovarRow = (KeySym, Row)
	type CovarMatrix = Seq[CovarRow]

	def projectCovarMatrix(keySyms : Seq[KeySym], maxLevels : Int) : CovarMatrix // Use at most maxLevels of the bin structure
	def projectRootBin(keySyms : Seq[KeySym]) : DBinDat // Often this is same as mean.
	def projectChildBins(parentBinID : DBinID, keySyms : Seq[KeySym]) : Seq[DBinDat]
}


// Assume meatKeys are same across all bins
class VecDistribImpl(rootBN : BinNode) extends VecDistrib {
	override def getFullKeySymSet: Set[KeySym] = rootBN.getFullKeySymSet

	override def projectMean(keySyms: Seq[KeySym]): Row = rootBN.projectMean(keySyms)

	override def projectCovarMatrix(keySyms: Seq[KeySym], maxLevels: DBinID): CovarMatrix = ???

	override def projectRootBin(keySyms: Seq[KeySym]): DBinDat = rootBN.projectToDBD(keySyms)

	override def projectChildBins(parentBinID: DBinID, keySyms: Seq[KeySym]): Seq[DBinDat] = ???
}

trait ToBinData {
	def pullTimeData(itm : Item) : BinTimeData = {
		// If field is missing, we throw?  Or should we return option/either?
		???
	}

}
trait GenBinData {

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
