package fun.gravax.axlp.full

import fun.gravax.aa.yafl.core.{YaflAltSum, YaflCartProd}
import fun.gravax.aa.yafl.full.YaflUri
import fun.gravax.axlp.core.num.{PureNum, WholeIntPN}
import fun.gravax.axlp.core.struct.FinListN

private trait PureSymAlignStuff

/*
Define alignment from pure types to some well known URIs.

AxLam is a dependent-typed Lambda calculus (with universes like K-DTT TODO: Elaborate)

These AxLam types are instantiatable and usable for runtime computations.
AxLam provides functors around the more abstract Yafl types, which are not explicitly used in computation.

AxLam is encodable as RDF following AxLam-Schema, Data easily translates to JSON, also Parquet.
AxLam functions usually use JSON for input+output, while quietly reading from some (cachable) RDF.

AxLam composite functions are defined via RDF graphs.
Primitive functions are declared with RDF names and implemented in a code library.
Regarding purity and totality of a particular functions, proofs may be offered.

Primitive functions must be treated as unreliable by any system seeking to use AxLam securely,
which is probably a bad idea.
For experimental usage, existing result data may be
loaded from RDF and treated as "known".
The "code" of AxLam funcs always comes from "known" result data.

AxLam services are built using AxLamImpl runtime methods for "query" and "eval".
Proceeding from general to specific:
Query checks only "known" facts, without computing anything.
// Pretend our cache source has a friendly local name URI (which could be 'g' name in a quadstore)
val cacheSetupURI = "goodApp:cacheAssumptionsForOurRun"
val ctxWithResultCache = cacheBoss.prepareCache(cacheSetupURI)
// load known result triples usable and useful for this invocation.
// when we have locality (e.g. the subjPred pairs are unsurprising, because the cache is warm in the areas of the query)
 this need may come from local fast ram cache be fulfilled by any KG subset
val ctxWithResultCache = ctxWithKGs.loadKnownResultTriples(subjPredPairsUnsorted)
val knownResultTriplesUnsorted = ctxWithResultCache.queryKnownResults(subjPredPairsUnsorted)
// Now that above result can be checked by caller for matches, and caller may then
// determine whether an eval is desirable, and if so, whether those results might
// be somehow stored or output.  However the eval itself must be absolutely pure,
// and preferably bounded in RAM usage.   Eval may only be performed for a single
// AxLam function evaluation at a time.  It may not be performed as a "sequence"
// because such sequences may not be parallelized.
// Generally the Eval runtime + primitives should be written to ensure that eval is
// pure.
// Eval should always return an equivalent resultObject for the same input, within a governing definitional context.

// When the subj+pred call has a single result, this result is presumed to be pred(subj), which by definition is a
// functional result.

// return a diff
//
// When the eval produces zero or multiple results, it indicates some that the invocation was not for a true function.

// Instead the predicate represents some relation

val freshRsltItems = evalFreshResult(subjPredPair)


Eval has runtime-specific behavior governed by the following ruleset:


properties may act like functions
functions take one arg but are eval in a Ctx
a function invocation may be recorded as a triple
     :input  :functionPred  :result
then a collection of such triples may be seen as a (partial-)result cache

if :functionPred is truly a math function of ths form, then it must have
a single unique :result (from the codomain) for any given :input (from
the domain).  From here we apply terminology:
  function: inclusion,
  domain:  fibres, preimage
  codomain:  image

     :pipn_8  :cubeRoot :pipn_pos2
     :pipn_9  :sqRoot   :branch_pair(:pipn_pos3  :nipn_neg3)
 */



// All these datatypes are intended to be immutable and pure

// A DataThing always has a type representable by a String, which *may* encode parametric information,
// tho not looking to go wild with that.
// Note that we should not yet simply say "DataThing extends YaflCoreDat" even though we intend it to be
// approximately true, because we do not yet wish to promise that every DataThing is known squeaky clean.
// Instead we pull in specific subtypes of YaflCoreDat as markers on particular subtypes of DataThing.
trait DataThing {
	// A DataThing instance "thinks of itself" as having a single instType with a fixed uri-name.
	// We may see this method as a function from domain=DataThing to codomain=UriThing
	val instTypeURI : UriThing
	// However, this DataThing may be equivalent to other data things which are of other types, or use other
	// names for an equivalent type.
	// For example, 4 : PositiveInteger is 'equivalent' to 4 : Int_GreaterThan_2 and to 4 : Rat_LessThan_5.8.
	// In this sense of equivalence, 4 is of infinitely many distinct types (because it satisfies many propositions).
	// In RDF/OWL these could all be the same Individual named :ourNum_4.
	// But a particular DataThing runtime instance ourNumDataThing_4 can have only one named type which
	// is fixed at instTypeURI.  This instTypeUri may then be used if we wish to encode the DataThing
	// as JSON or RDF.  For JSON output we expect any downstream component (outside Axiomagic) will likely
	// take our assigned type for granted (or "granite" cf. Morty's uncle Rick).
	// To summarize: JSON is for Mortys, RDF is for Ricks.
}

trait RdfNodeThing extends DataThing
trait RdfRsrcThing extends RdfNodeThing // Often a Uri, but may be a BNode
trait UriThing extends RdfRsrcThing with YaflUri {
	val absUriTxt : String
}
trait RdfBNodeThing extends RdfRsrcThing {
}
trait RdfLiteralThing extends RdfNodeThing
// Do we make scala subtypes for specific RDF-literal types?

// Are we able to pass literals (Strings, Numbers, Dates) in the triple-subject i.e. input position?
// (Suggestive of "Generalized Triples" in RDF 1.1).
// Or must we wrap them into

// May convert to/from TEXT and PURE_NUM types.
// Should there also then be Json-specific thing types?
object FixedTypeURIs {
	val FTUTXT_EMPTY = UriData("uri:axtyp:EMPTY")
	val FTUTXT_URI = UriData("uri:axtyp:URI") // Subtypes:  URI that satisfies some constraint
	val FTUTXT_TEXT = UriData("uri:axtyp:TEXT") // What about subtypes:  Text that satisfies some constraints
	val FTUTXT_PURE_NUM = UriData("uri:axtyp:PURE_NUM") // Starts to get tricky here, because we do want subtypes
}
class ScalarDataThing(myIntensionalTypeURI : UriData) extends DataThing {
	override val instTypeURI = myIntensionalTypeURI
}
// No DataThing may wrap or return java-null.
// Someone seeking to "hack axLam" could seek to violate this rule.
// We seek to ensure that the common bad things which can happen in axLam are either unexpected EmptyData, or
// unexpected undecidable/non-terminating/"slow" function.
// Then we seek to give tools for constructing systems which don't do either of those things (unexpectedly).
// From our coding perspective here, "null" should really not happen.  Can write 10 books about all that, natch ;-)
case class EmptyData() extends ScalarDataThing(FixedTypeURIs.FTUTXT_EMPTY)

// uriTxt may not be null!
case class UriData(myUriTxt: String) extends ScalarDataThing(FixedTypeURIs.FTUTXT_URI) with UriThing {
	override val absUriTxt = myUriTxt
}

// myTxt may not be null!
case class TextData(myTxt : String) extends ScalarDataThing(FixedTypeURIs.FTUTXT_TEXT)

// myPN may not be null!
case class PureNumData(myPN : PureNum) extends ScalarDataThing(FixedTypeURIs.FTUTXT_PURE_NUM)

// The other types of DataThing are built algebraically over the above, amenable to formal analysis
// 		1) Cartesian products (Tuples in Scala, collections of known size) typed as dependent products,
// 		logically conjunctions, quantified as forAll
//		2) Dependent sums (Eithers in Scala, collections of variable size), logical disjunctions, quantified as exists
//		3) Subsets defined by a proposition over any other set-from-type.  Example:  IsoscelesTriangle
//			Consider a case class with a tuple as single constructor parameter

// We can see that ProductData and UnionData *types* are each defined in terms of a fixed list of other *types.

// Interop with JSON + RDF occurs via the action of these machine-side types.
// Build up records from these, will be spread transparently across RDF preds or json-props.
// Meanwhile OuterType is a logic-side strong-ish language-native type like Tuple3[TextData, PureNumData, ListMaxN[ProductData[Tuple2[TextData...
trait CartesianProdData[NuggetType <: YaflCartProd] extends DataThing
{
	// How many elements in this cartesian product? -- this is known at the type level, e.g. if OuterType is Tuple7 then 7
	// A product of width 0 is provably equal to EmptyData().
	// This product is ordered.
	val getProductWidth : WholeIntPN
	def getNugget : NuggetType // This object represents an actual cartesian-product value, usually as a TupleN[T1..TN]
	// Want some way to access the types of the fields
}
// Input data may not be null, nor contain nulls.
// FieldNames must correspond exactly to the tuple-positions of pdat.
abstract class RecordAccessor[OT <: YaflCartProd](fieldNames : FinListN[String], pdat : CartesianProdData[OT]) {
	def getItem (fname : String) : Any // FIXME: where's the type of the field?  It is inside the OT!
}

trait AltUnionData[NuggetType <: YaflAltSum] extends DataThing {
	// Recorded as some kind of union or variably-typed item.  Dependent sum in type theory.
	// We restrict to a fixed, finite number of incarnations, in a given order, without a name.
	// We call this number the depth of the Union.
	// A union of depth 0 is provably equal to EmptyData().
	// How many possible incarnations? Scala "Either" is 2.
	val getUnionDepth : WholeIntPN
	def getNugget : NuggetType
}


