package fun.gravax.dsktch.quantile

import org.apache.datasketches.quantiles.ItemsSketch

trait QuantileSketchReadWrapper[T] extends QuantileSketchReader[T] {

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

	// With all these methods that return arrays, where is guarantee that these returned arrays are immutable?

	override def getPMF(splitPoints: InArrT): Array[Double] = fetchWrappedSketch.getPMF(splitPoints)

	//  found   : Array[T] 	// required: Array[T with Object] //Note: T >: T with Object, but class Array is invariant in type T.
	override def getCDF(splitPoints: InArrT): Array[Double] = fetchWrappedSketch.getCDF(splitPoints)

	//  found   : Array[T with Object]   ...   but class Array is invariant in type T.
	override def getQuantiles(fRanks : Array[Double]): OutArrT = fetchWrappedSketch.getQuantiles(fRanks)

	override def getQuantiles(evenlySpaced : Int): OutArrT = fetchWrappedSketch.getQuantiles(evenlySpaced)

	override def getSummaryTxt(flg_includeSketchSummary: Boolean, flg_includeDataDetail: Boolean) : String = {
		fetchWrappedSketch.toString(flg_includeSketchSummary, flg_includeDataDetail)
	}
}

