package fun.gravax.zpol.dsktch

import org.apache.datasketches.quantiles.{ItemsSketch, ItemsUnion}

import java.util.Comparator

private trait FirstWhackAtDSktch

/*
Apache DataSketches use mutable accumulators.
We want an API that allows them to work with them cleanly from functional code.
Perhaps need to wrap in a State/Writer monad.
Treat as a resource to be opened before processing a clump.
How about wrapping as a ZStream sink?

 */

trait QuantileSketchReader[T] {
	type InArrT = Array[T with Object]
	type OutArrT = Array[_ <: T]

	def getK: Int
	def getN : Long
	def isEmpty: Boolean
	def getRetainedItems: Int
	def getMaxValue: T
	def getMinValue: T
	def getRank(t : T) : Double
	def getQuantile(fraction : Double): T
	def getQuantileLowerBound(fraction : Double): T
	def getQuantileUpperBound(fraction : Double): T
	def getNormalizedRankError(pmf : Boolean) : Double

	def getPMF(splitPoints: InArrT): Array[Double]
	def getCDF(splitPoints: InArrT): Array[Double]
	def getQuantiles(fRanks : Array[Double]): OutArrT
	def getQuantiles(evenlySpaced : Int): OutArrT

}
trait QuantileSketchReaderHasMut[T] extends QuantileSketchReader[T] {


	// Offers only the output features of the sketch.
	protected def getMutSketch : ItemsSketch[T]



	// Returns the configured value of K
	override def getK: Int = getMutSketch.getK
	// Returns the length of the input stream so far.
	override def getN : Long = getMutSketch.getN
	override def isEmpty: Boolean = getMutSketch.isEmpty
	// Computes the number of retained entries (samples) in the sketch
	override def getRetainedItems: Int = getMutSketch.getRetainedItems

	override def getMaxValue: T = getMutSketch.getMaxValue
	override def getMinValue: T = getMutSketch.getMinValue

	// Returns an approximation to the normalized (fractional) rank of the given value from 0 to 1 inclusive.
	override def getRank(t : T) : Double = getMutSketch.getRank(t)

	// This returns an approximation to the value of the data item that would be preceded by the given fraction of a hypothetical sorted version of the input stream so far.
	override def getQuantile(fraction : Double): T = getMutSketch.getQuantile(fraction)

	// Gets the lower bound of the value interval in which the true quantile of the given rank exists with a confidence of at least 99%.
	override def getQuantileLowerBound(fraction : Double): T = getMutSketch.getQuantileLowerBound(fraction)

	// Gets the upper bound of the value interval in which the true quantile of the given rank exists with a confidence of at least 99%.
	override def getQuantileUpperBound(fraction : Double): T = getMutSketch.getQuantileUpperBound(fraction)

	// Gets the approximate rank error of this sketch normalized as a fraction between zero and one.
	// pmf - if true, returns the "double-sided" normalized rank error for the getPMF() function.
	// Otherwise, it is the "single-sided" normalized rank error for all the other queries.
	override def getNormalizedRankError(pmf : Boolean) : Double = getMutSketch.getNormalizedRankError(pmf)

	// With all these methods that return arrays, where is guarantee that these returned arrays are immutable?


	/* getPMF     https://datasketches.apache.org/api/java/snapshot/apidocs/org/apache/datasketches/quantiles/ItemsSketch.html
Returns an approximation to the Probability Mass Function (PMF) of the input stream given a set of splitPoints (values).
The resulting approximations have a probabilistic guarantee that be obtained from the getNormalizedRankError(true) function.
If the sketch is empty this returns null.
Parameters:
splitPoints - an array of m unique, monotonically increasing item values that divide the ordered space into m+1 consecutive disjoint intervals. The definition of an "interval" is inclusive of the left splitPoint (or minimum value) and exclusive of the right splitPoint, with the exception that the last interval will include the maximum value. It is not necessary to include either the min or max values in these splitpoints.
Returns:
an array of m+1 doubles each of which is an approximation to the fraction of the input stream values (the mass) that fall into one of those intervals. The definition of an "interval" is inclusive of the left splitPoint and exclusive of the right splitPoint, with the exception that the last interval will include maximum value.
 */
	// public double[] getPMF(T[] splitPoints) {
	override def getPMF(splitPoints: InArrT): Array[Double] = getMutSketch.getPMF(splitPoints)


	/* getCDF     https://datasketches.apache.org/api/java/snapshot/apidocs/org/apache/datasketches/quantiles/ItemsSketch.html
Returns an approximation to the Cumulative Distribution Function (CDF), which is the cumulative analog of the PMF, of the input stream given a set of splitPoints (values).
The resulting approximations have a probabilistic guarantee that be obtained from the getNormalizedRankError(false) function.
If the sketch is empty this returns null.

The value at array position j of the returned CDF array is the sum of the returned values in positions 0 through j of the returned PMF array.
	 */
	//  found   : Array[T] 	// required: Array[T with Object] //Note: T >: T with Object, but class Array is invariant in type T.
	override def getCDF(splitPoints: InArrT): Array[Double] = getMutSketch.getCDF(splitPoints)

/*
	This is a more efficient multiple-query version of getQuantile().
			This returns an array that could have been generated by using getQuantile() with many different fractional ranks, but would be very inefficient. This method incurs the internal set-up overhead once and obtains multiple quantile values in a single query. It is strongly recommend that this method be used instead of multiple calls to getQuantile().

			If the sketch is empty this returns null.

			Parameters:
			fRanks - the given array of fractional (or normalized) ranks in the hypothetical sorted stream of all the input values seen so far.
			These fRanks must all be in the interval [0.0, 1.0] inclusively.
			Returns:
			array of approximate quantiles of the given fRanks in the same order as in the given fRanks array.

 */
	//  found   : Array[T with Object]   ...   but class Array is invariant in type T.
	override def getQuantiles(fRanks : Array[Double]): OutArrT = getMutSketch.getQuantiles(fRanks)
/*
evenlySpaced - an integer that specifies the number of evenly spaced fractional ranks.
This must be a positive integer greater than 1. A value of 2 will return the min and the max value.
A value of 3 will return the min, the median and the max value, etc.
 */
	override def getQuantiles(evenlySpaced : Int): OutArrT = getMutSketch.getQuantiles(evenlySpaced)

}

trait QuantileSketchWriter[T] {
	// This writer contains the (approximate) result of items written so far.
	// The writer does not allow mutation.  If you call ".addItem" twice (on THIS writer), you will get two different
	// QSWs out. This usually implies copying costs.  Note that we could make a lossless QSW that always keeps all items.
	def addItem(itm : T) : QuantileSketchWriter[T]
	// Merge produces a new writer.
	def merge(otherQSW : QuantileSketchWriter[T]) : QuantileSketchWriter[T]
	// Return an immutable sketch reader
	def getSketchReader : QuantileSketchReader[T]
}

case class EmptyQSW[T](numBins : Int, compr : Comparator[_ >: T]) extends QuantileSketchWriter[T] {
	override def addItem(itm: T): QuantileSketchWriter[T] = {
		val sktchOfOne: ItemsSketch[T] = ItemsSketch.getInstance[T](numBins, compr)
		sktchOfOne.update(itm)
		new QntlSktchWrtrImplHeavy[T](sktchOfOne)
	}

	override def merge(otherQSW: QuantileSketchWriter[T]): QuantileSketchWriter[T] =  ???

	override def getSketchReader: QuantileSketchReader[T] = ???
}

class QntlSktchWrtrImplHeavy[T](val mutSktch : ItemsSketch[T]) extends QuantileSketchWriter[T] {
	override def addItem(itm: T): QuantileSketchWriter[T] = {
		val helpyUnion = ItemsUnion.getInstance(mutSktch)
		helpyUnion.update(itm)
		val outSktch: ItemsSketch[T] = helpyUnion.getResult
		val outWrtr = new QntlSktchWrtrImplHeavy(outSktch)
		outWrtr
	}

	override def merge(otherQSW: QuantileSketchWriter[T]): QuantileSketchWriter[T] = {
		val union = ItemsUnion.getInstance(mutSktch)
		otherQSW match {
			case hvy : QntlSktchWrtrImplHeavy[T] => union.update(hvy.mutSktch)
			case _ => throw new Exception(s"Cannot merge with otherQSW of class ${otherQSW.getClass}")
		}
		val outSktch: ItemsSketch[T] = union.getResult
		val outWrtr = new QntlSktchWrtrImplHeavy(outSktch)
		outWrtr
	}

	override def getSketchReader: QuantileSketchReader[T] = {
		val msktch = mutSktch
		new QuantileSketchReaderHasMut[T] {
			override protected def getMutSketch: ItemsSketch[T] = msktch
		}
	}
}
trait HeavySktchMkr {
	def mkEmptyQSW[T](numBins : Int, compr : Comparator[_ >: T]) : QuantileSketchWriter[T] = EmptyQSW(numBins, compr)
}

trait MakeSomeQuantSktchs {
	def go : Unit = {
		val hsm = new HeavySktchMkr {}
		val binCnt = 8
		// TODO: Consider different syntax forms for defining Comparator in Scala
		val bdComp = new Comparator[BigDecimal] {
			override def compare(o1: BigDecimal, o2: BigDecimal): Int = o1.compare(o2)
		}
		def qswBD = hsm.mkEmptyQSW(8, bdComp)

	}
}