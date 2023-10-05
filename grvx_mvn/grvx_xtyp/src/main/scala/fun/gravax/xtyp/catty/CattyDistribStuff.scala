package fun.gravax.xtyp.catty

private trait CattyDistribStuff {

}
// In LazyPPL there are two monads:  Prob and Meas

// An outcomeSet is an element of a (quasi-)borel set.
// The most common form
// Event is the type of allowed outcomes. Each outcome can have a computed probability in a distribution instance.
trait Distrib[OutcomeQBS,Meas] {
	// This is an OOP framing as a service-like object, rather than a data object.
	// Get a probability measure for a given set
	def getMeasureForSet(outcomeSet : OutcomeQBS) : Meas
}
trait DistribFuncs {


}
/**
 * From ""A compositional approach to scalable Bayesian computation and probabilistic programming".
http://www.mas.ncl.ac.uk/~ndjw1/docs/djw-acmll.pdf
• A Rand[T] represents a random quantity of type T
• It is used to encapsulate the non-determinism of functions
returning random quantities — otherwise these would break
the purity and referential transparency of the function
• map is used to transform one random quantity into another
• flatMap is used to chain together stochastic functions to
create joint and/or marginal random variables, or to propagate
uncertainty through a computational work-ﬂow or pipeline
 */
