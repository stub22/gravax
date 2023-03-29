package fun.gravax.dsktch.quantile

import org.apache.datasketches.quantiles.ItemsSketch

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

/***
 *
 * @param classTag$T$0
 * @tparam T
 * Scala doesn't much like Array[T] as a type, at least when Scala 2.13 is calling Java 8.
 * The <: Object type bound is needed for variance compatibility with array methods of ItemSketch[T].
 * Meanwhile the :ClassTag context bound allows us to carry implicit denoting concrete runtime type of T to the array
 * constructors used by getPMF, getCDF.
 */

abstract class QuantileSketchReadWrapper[T <: Object : ClassTag] extends QuantileSketchReader[T] {

	// This Wrapper trait implements all the Reader-API methods by delegating to this internal sketch.
	// We expect that this internal sketch should not change, but enforcement of that rule depends on subclass impls.
	protected def fetchWrappedSketch : ItemsSketch[T]

	override def getK: Int = fetchWrappedSketch.getK

	override def getN : Long = fetchWrappedSketch.getN

	override def isEmpty: Boolean = fetchWrappedSketch.isEmpty

	override def getRetainedItems: Int = fetchWrappedSketch.getRetainedItems

	override def getMaxValue: T = fetchWrappedSketch.getMaxValue
	override def getMinValue: T = fetchWrappedSketch.getMinValue

	override def getRank(t : T) : Double = fetchWrappedSketch.getRank(t)

	override def getQuantile(fraction : Double): T = fetchWrappedSketch.getQuantile(fraction)

	override def getQuantileLowerBound(fraction : Double): T = fetchWrappedSketch.getQuantileLowerBound(fraction)

	override def getQuantileUpperBound(fraction : Double): T = fetchWrappedSketch.getQuantileUpperBound(fraction)

	override def getNormalizedRankError(pmf : Boolean) : Double = fetchWrappedSketch.getNormalizedRankError(pmf)

	override def getSummaryTxt(flg_includeSketchSummary: Boolean, flg_includeDataDetail: Boolean) : String = {
		fetchWrappedSketch.toString(flg_includeSketchSummary, flg_includeDataDetail)
	}

	def flg_doCopyIn : Boolean = true
	def flg_doCopyOut : Boolean = true

	// Appears that ArrayOps.toSeq does NOT make a copy
	override def getPMF(splitPoints: Seq[T]): Seq[RealNum] = {
		val inArr : Array[T] = seqTtoArrT(splitPoints)
		val outArr: Array[RealNum] = getPMF_Arr(inArr)
		val outSeq : OutSeqRN = arrDtoSeqRN(outArr)
		outSeq
	}

	override def getCDF(splitPoints: Seq[T]): Seq[RealNum] = {
		val inArr : Array[T] = seqTtoArrT(splitPoints)
		val outArr: Array[Double] = getCDF_Arr(inArr)
		val outSeq : OutSeqRN = arrDtoSeqRN(outArr)
		// val outSeq : OutSeqRN = if (flg_doCopyOut)	ArraySeq.from(outArr) else outArr
		outSeq
	}

	override def getQuantiles(fRanks: Seq[RealNum]): Seq[T] = {
		val inArr = seqRNtoArrD(fRanks)
		val outArr = getQuantiles_Arr(inArr)
		val outSeq = arrTtoSeqT(outArr)
		outSeq
	}

	override def getQuantiles(evenlySpaced: Int): Seq[T] = {
		val outArrT  = getQuantiles_Arr(evenlySpaced)
		val outSeq = arrTtoSeqT(outArrT)
		outSeq
	}

	private def seqTtoArrT(seqT: Seq[T]) : Array[T] = {
		if (flg_doCopyIn)
			Array.from(seqT)
		else seqT.toArray
	}
	private def arrTtoSeqT(arrT: Array[T]) : Seq[T] = {
		if (flg_doCopyOut)	ArraySeq.from(arrT) else arrT
	}
	private def seqRNtoArrD(seqRN: Seq[RealNum]) : Array[Double] = {
		if (flg_doCopyIn)
			Array.from(seqRN)
		else seqRN.toArray
	}
	private def arrDtoSeqRN(arrRN: Array[Double]) : Seq[RealNum] = {
		if (flg_doCopyOut)	ArraySeq.from(arrRN) else arrRN
	}

	// With all these methods that return arrays, where is guarantee that these returned arrays are immutable?
	protected def getPMF_Arr(splitPoints: Array[T]): Array[Double] = fetchWrappedSketch.getPMF(splitPoints)

	//  found   : Array[T] 	// required: Array[T with Object] //Note: T >: T with Object, but class Array is invariant in type T.
	protected def getCDF_Arr(splitPoints: Array[T]): Array[Double] = fetchWrappedSketch.getCDF(splitPoints)

	//  found   : Array[T with Object]   ...   but class Array is invariant in type T.
	protected def getQuantiles_Arr(fRanks : Array[Double]): Array[T] = fetchWrappedSketch.getQuantiles(fRanks)

	protected def getQuantiles_Arr(evenlySpaced : Int): Array[T] = fetchWrappedSketch.getQuantiles(evenlySpaced)

}

