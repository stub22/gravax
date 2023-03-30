package fun.gravax.dsktch.quantile

import org.apache.datasketches.quantiles.{ItemsSketch, ItemsUnion}

import java.util.Comparator
import scala.reflect.ClassTag

/*

We want an API that allows them to work with them cleanly from functional code.
Perhaps need to wrap in a State/Writer monad.
Treat as a resource to be opened before processing a clump.
How about wrapping as a ZStream sink?

 */

trait QuantileSketchWrapperMaker {
	protected def getFlg_UseHotWriter : Boolean = false
	protected def getFlg_UseBuffWriter : Boolean = true
	protected def getBufferLimit : Int = 32
	//  K must be >= 2 and <= 32768 and a power of 2
	def mkEmptyWriteWrapper[T <: Object : ClassTag](numBins : Int, compr : Comparator[_ >: T]) : QuantileSketchWriter[T] = {
		val emptySketch = ItemsSketch.getInstance[T](numBins, compr)
		writerForExistingSketch(emptySketch)
	}

	def writerForExistingSketch[T <: Object : ClassTag](mutSktch : ItemsSketch[T]) : QuantileSketchWriter[T] = {
		if (getFlg_UseHotWriter)
			new QSWW_Hot(mutSktch)
		else {
			val baseQSWW = new QuantileSketchWriteWrapper(mutSktch)
			if (getFlg_UseBuffWriter) {
				new QSWW_Buffered[T](baseQSWW, Nil, getBufferLimit)
			} else baseQSWW
		}
	}

	def readerForExistingSketch[T <: Object : ClassTag](existingMutSktch : ItemsSketch[T]) : QuantileSketchReader[T] = {
		???
	}
}

