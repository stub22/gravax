package fun.gravax.dsktch.quantile

import org.apache.datasketches.quantiles.{ItemsSketch, ItemsUnion}

import scala.reflect.ClassTag

/***
 * Pure functional wrapper of Apache DataSketches ItemSketches.
 * Each ItemSketch uses a mutable accumulators.
 * On each mutation operation we create a *new* Wrapper holding a *new* ItemSketch.
 * @param myMutSktch
 * @param classTag$T$0
 * @tparam T
 */
//
class QuantileSketchWriteWrapper[T <: Object : ClassTag](protected val myMutSktch : ItemsSketch[T]) extends QuantileSketchWriter[T] {
	override def addItem(itm: T): QuantileSketchWriter[T] = {
		val helpyUnion = ItemsUnion.getInstance(myMutSktch)
		helpyUnion.update(itm)
		val outSktch: ItemsSketch[T] = helpyUnion.getResult
		val outWrtr = new QuantileSketchWriteWrapper(outSktch)
		outWrtr
	}

	override def addItems(items: IterableOnce[T]): QuantileSketchWriter[T] = {
		// Adding multiple items in one step will generally be faster than calling .addItem many times, because
		// here we are able to get by with copying the internal sketch only ONCE.
		val helpyUnion = ItemsUnion.getInstance(myMutSktch)
		items.iterator.foreach(itm => helpyUnion.update(itm))
		val outSktch: ItemsSketch[T] = helpyUnion.getResult
		val outWrtr = new QuantileSketchWriteWrapper(outSktch)
		outWrtr
	}

	override def checkMergeCompat(otherQSW: QuantileSketchWriter[T]): Boolean = {
		// TODO:  Confirm that K values are compatible
		otherQSW.isInstanceOf[QuantileSketchWriteWrapper[T]]
	}


	override def mergeIfCompat(otherQSW: QuantileSketchWriter[T]): Option[QuantileSketchWriter[T]] = {
		otherQSW match {
			case otherWrap : QuantileSketchWriteWrapper[T] => {
				val union: ItemsUnion[T] = ItemsUnion.getInstance(myMutSktch)
				union.update(otherWrap.myMutSktch)
				val outSktch: ItemsSketch[T] = union.getResult
				val outWrap = new QuantileSketchWriteWrapper(outSktch)
				Some(outWrap)
			}
			case _ => None // throw new Exception(s"Cannot merge with otherQSW of class ${otherQSW.getClass}")
		}

	}

	private lazy val mySktchReader = new QuantileSketchReadWrapper[T] {
		override protected def fetchWrappedSketch: ItemsSketch[T] = myMutSktch
	}

	override def getSketchReader: QuantileSketchReader[T] = mySktchReader


}

class QSWW_Hot[T <: Object : ClassTag](sktch : ItemsSketch[T]) extends QuantileSketchWriteWrapper[T](sktch) {
	override def addItem(itm: T): QuantileSketchWriter[T] = {
		myMutSktch.update(itm)
		this
	}
}

class QSWW_Buffered[T <: Object : ClassTag](prevQSW : QuantileSketchWriter[T], pendingItemsRev : List[T], buffLim : Int) extends QuantileSketchWriter[T] {
	// Allowing this value to be memoized
	private lazy val myCondensedQSW = prevQSW.addItems(pendingItemsRev.reverse)

	private def getCondensedQSW = myCondensedQSW

	override def addItem(item: T): QuantileSketchWriter[T] = {
		val bufLen = pendingItemsRev.length
		if (bufLen + 1 >= buffLim)
			new QSWW_Buffered(myCondensedQSW, List(item), buffLim)
		else
			new QSWW_Buffered(prevQSW, item :: pendingItemsRev, buffLim)
	}

	override def addItems(items: IterableOnce[T]): QuantileSketchWriter[T] = {
		// There are different ways to approach this absorbtion, depending on our guesses about how large the buffers
		// are, and the likelihood that .getSketchReader will be called.
		val revItmLst = items.iterator.toList.reverse
		val revItmCnt = revItmLst.length
		val pendItmCnt = pendingItemsRev.length

		if (revItmCnt + pendItmCnt >= buffLim)
			new QSWW_Buffered(myCondensedQSW, revItmLst, buffLim)
		else {
			val expBufList = revItmLst ::: pendingItemsRev
			new QSWW_Buffered[T](prevQSW, expBufList, buffLim)
		}
	}

	// Our fetching here forces the lazy condensedQSW to be built and memoized.
	override def getSketchReader: QuantileSketchReader[T] = myCondensedQSW.getSketchReader

	override def mergeIfCompat(otherQSW: QuantileSketchWriter[T]): Option[QuantileSketchWriter[T]] = {
		val ourCondQSW = getCondensedQSW
		otherQSW match {
			case otherQSWB : QSWW_Buffered[T] => ourCondQSW.mergeIfCompat(otherQSWB.myCondensedQSW)
			case _ => None
		}
	}

	override def checkMergeCompat(otherQSW: QuantileSketchWriter[T]): Boolean = {
		otherQSW.isInstanceOf[QSWW_Buffered[T]]
	}
}