package fun.gravax.dsktch.quantile

import org.apache.datasketches.quantiles.{ItemsSketch, ItemsUnion}

import java.util.Comparator
import scala.reflect.ClassTag

/*
Apache DataSketches use mutable accumulators.
We want an API that allows them to work with them cleanly from functional code.
Perhaps need to wrap in a State/Writer monad.
Treat as a resource to be opened before processing a clump.
How about wrapping as a ZStream sink?

 */

trait QuantileSketchWrapperMaker {
	//  K must be >= 2 and <= 32768 and a power of 2
	def mkEmptyWriteWrapper[T <: Object : ClassTag](numBins : Int, compr : Comparator[_ >: T]) : QuantileSketchWriter[T] = {
		val emptySketch = ItemsSketch.getInstance[T](numBins, compr)
		new QuantileSketchWriteWrapper(emptySketch)
	}

	def writerForExistingSketch[T <: Object : ClassTag](existingMutSktch : ItemsSketch[T]) : QuantileSketchWriter[T] = {
		new QuantileSketchWriteWrapper(existingMutSktch)
	}

	def readerForExistingSketch[T <: Object : ClassTag](existingMutSktch : ItemsSketch[T]) : QuantileSketchReader[T] = {
		???
	}
}

