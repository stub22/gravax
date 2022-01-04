package fun.gravax.axlp.full

private trait PureFuncStuff

trait EvalCtx {
	type Subj = RdfRsrcThing
	type Pred = UriThing
	type Rslt = Set[RdfNodeThing]
	def evalFreshResult(inPair: (Subj,Pred)) : Option[Rslt]
	// None => computational failure (timeout, RAM error, ...), which proves nothing about existence of function result.
	// Diagnosis is not available at this level.
	// None should NOT be interpreted as "NO RESULTS FOUND", i.e. None is not the same as Some(EmptyResult).
}

// These funcs are typically instantiated within some useful context that supplies wiring implicitly.
trait AxLamRelation {
	val funcURI : UriData 	// This URI is used in the subject position of a function definition,
							// and in the prop position of an invocation.
							// The function input must always be an RdfRsrc

	// domain type description
	val inputTypeURI : UriData // Constrained to be a subtype of a resource (referring to a ThingData)
	// codomain type description
	val outputTypeURI : UriData // Constrained as some subtype of RdfNodeThing
	def computeResults(inRsrc : RdfRsrcThing) : Option[Set[RdfNodeThing]]
}
trait AxLamFunction extends AxLamRelation {
	// Known to produce a singleton result
	// None indicates computation failure or abort.  No logical significance.  Could be retried.
	def computeSingleResult(inRsrc : RdfRsrcThing) : Option[RdfNodeThing]
}
trait CompositeFunc

