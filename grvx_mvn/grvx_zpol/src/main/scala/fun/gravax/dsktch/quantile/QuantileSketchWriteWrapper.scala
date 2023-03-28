package fun.gravax.dsktch.quantile

import org.apache.datasketches.quantiles.{ItemsSketch, ItemsUnion}

class QuantileSketchWriteWrapper[T](val mutSktch : ItemsSketch[T]) extends QuantileSketchWriter[T] {
	override def addItem(itm: T): QuantileSketchWriter[T] = {
		val helpyUnion = ItemsUnion.getInstance(mutSktch)
		helpyUnion.update(itm)
		val outSktch: ItemsSketch[T] = helpyUnion.getResult
		val outWrtr = new QuantileSketchWriteWrapper(outSktch)
		outWrtr
	}

	override def addItems(items: IterableOnce[T]): QuantileSketchWriter[T] = ???

	override def merge(otherQSW: QuantileSketchWriter[T]): QuantileSketchWriter[T] = {
		val union = ItemsUnion.getInstance(mutSktch)
		otherQSW match {
			case hvy : QuantileSketchWriteWrapper[T] => union.update(hvy.mutSktch)
			case _ => throw new Exception(s"Cannot merge with otherQSW of class ${otherQSW.getClass}")
		}
		val outSktch: ItemsSketch[T] = union.getResult
		val outWrtr = new QuantileSketchWriteWrapper(outSktch)
		outWrtr
	}

	override def getSketchReader: QuantileSketchReader[T] = {
		val msktch = mutSktch
		new QuantileSketchReadWrapper[T] {
			override protected def fetchWrappedSketch: ItemsSketch[T] = msktch
		}
	}

}
