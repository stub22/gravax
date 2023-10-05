package fun.gravax.pred

object RunHelloPredspc {

}

// A Cond is used to assert a condition that COULD hold.
// A CondCtx indicates a chain of Conds that are PRESUMED to hold, within the context.
// Within a CondCtx we may discuss the likelihood that various Conds hold.

// Conditions later in the chain may not contradict earlier ones, but may cause us to re-interpret them

// A single Cond might tell us the price of one asset at one time, or it might tell us the
// prices of 1000s of assets across 1000s of times.
trait PredMkr {
	type Cond
	type CondCtx

	// Appends a Cond
	def assert(condCtx: CondCtx, assertionBody: Cond): CondCtx

	// body
	def chain(bodyMaker: Function1[CondCtx, Cond]): Function1[CondCtx, CondCtx]

}

trait GoodCondCtx {
	// Users query for distribution of a set of random variables
	// Each random variable has a name and a type, which are known to the user.
	type RVDesc
	// type RVNameSet = Set[RVName]
	type Distrib
	// Establishing this sequence allows the Distirbution to offer positional API
	def queryDistrib(rvDescs : Seq[RVDesc]) : Distrib
}

trait CubicalCondCtx extends GoodCondCtx {
	type RName = String
	type RValue // We really want this to vary according to the RName, which would require something like Shapeless HList.
	type ProbMeasureVal

	// Define range bounds for each value.
	type RangeBound = (RName, RValue, RValue)
	// The product of a bunch of range bounds is a region of space.
	type SpaceChunk = Seq[RangeBound]
	override type Distrib = Function1[SpaceChunk,ProbMeasureVal]

	// Now we can stream a bunch of spaceChunks through the Distrib func and get a stream of probMeasureVals.
}

class ChunkCondCtx extends CubicalCondCtx {
	override type RValue = String
	override type ProbMeasureVal = String
	override type RVDesc = String

	override def queryDistrib(rvDescs: Seq[RVDesc]): SpaceChunk => ProbMeasureVal = ???
}

class ChunkyPredMkr extends PredMkr {
	override type CondCtx = CubicalCondCtx
	override type Cond = String

	override def assert(condCtx: CubicalCondCtx, assertionBody: Cond): CubicalCondCtx = ???

	override def chain(bodyMaker: CubicalCondCtx => Cond): CubicalCondCtx => CubicalCondCtx = ???
}
