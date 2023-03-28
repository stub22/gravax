package fun.gravax.dsktch.quantile

trait QuantileSketchWriter[T] {
	// This writer contains the (approximate) result of items written so far.
	// The writer does not allow mutation.  If you call ".addItem" twice (on THIS writer), you will get two different
	// QSWs out. This may imply significant copying costs, depending on implementation.
	// Note that we COULD make a lossless QSW that always keeps all items.
	def addItem(item : T) : QuantileSketchWriter[T]

	// Here is a novel operation (not included in original datasketches mutable API) that gets us some extra
	// efficiency while still providing immutability guarantees to API user.  In a streaming context, this operation
	// can be used to add all the items from the current "Chunk" in one go.
	def addItems(items : IterableOnce[T]) : QuantileSketchWriter[T]

	// Merge items with another writer to produce a new writer that holds a sketch for all items.
	def merge(otherQSW : QuantileSketchWriter[T]) : QuantileSketchWriter[T]

	// Return an immutable sketch reader, based on the same set of items known by this writer.
	def getSketchReader : QuantileSketchReader[T]
}