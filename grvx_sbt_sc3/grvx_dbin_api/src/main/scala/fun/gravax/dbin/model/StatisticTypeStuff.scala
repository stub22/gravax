package fun.gravax.dbin.model

private trait StatisticTypeStuff

/*
An Entry is one of the (scalar) random variables in a multivariate distribution.
We may also interpet Entry as one of the dimensions of a random vector.
A Vector of Entries is a sample vector.
Entries are indexed by EntryKey.
The EntryXyz values we work with are always scalar *numbers*.  Only real numbers have been considered so far.
We use a functional approach to restrain the amount of assumptions we are making about representation and computation
with these values.
 */
trait EntryStatTypes {

	type EntryKey

	// Over any *part* of a distribution, these summary values can be computed for an entry.
	type EntryMean
	type EntryVariance
	type EntryExpectedSquare

	// Over any part, for any pair of entries.
	type EntryPairProduct

	// Requires the global means, does not have the same locality property as other stats above.
	type EntryPairCovar

	// A vector of some values, keyed by EntryKey
	type EVec[X]

	// Matrix of values, keyed by *pair* of EntryKey
	type EMatrix[X]

	// A number used to multiply the value of an entry, i.e. a coefficient.
	type EntryMult // Multiplier (not 'weight', which is only for the bin in this vocab) e.g. fraction of portfolio
}
trait EntryOps extends EntryStatTypes {
	
}