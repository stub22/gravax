package test.gravax.xtyp.histo

import fun.gravax.xtyp.mathy.{FlavorCollector, TSRFlavorTag}
import fun.gravax.xtyp.mathy.tridesc.TriShape
import org.scalatest.flatspec.AnyFlatSpec

private trait SpecsForExactTriangles

class FirstExactTriSpec extends AnyFlatSpec {
	"A triangle-shape-desc factory" should "make some random tri-shape-descs" in {

	}
}
/***
// Want to be able to classify and count any arbitrarily large set of TriShapes
// 1) According to their flavors, as a histogram (must account for flavor overlaps) of shape-desc counts per-flavor.
// This is easy to reduce on, since number of bins is known, fixed, small.
//
// 2) Partitioned by congruence or similarity class, with output of the total number of equiv-classes,
// and total shape-descs in the most populous equiv-classes.   Perhaps additional results consolidation,
// where the equiv-classes are further unified.  May also compute total area and perimiter of matching shapes.
// Area is interesting because algebraic num.
// Total area/perim/count of all shapes must be same regardless of how shapes are partitioned into classes.
// This is harder to reduce on, since there may be any number of equiv-class bins.

 Population of triShapes may come from any source, such as:
	handy collection, may be finite/infinite/unknown, strict or lazy
 	synchronousGenerator.nextTri()  which is sorta equiv to LazyList.  syncGen may be stateless or stateful.
	asyncGen posting into some stream/space  (such as an fs2 stream with backpressure)
	read from some file/resource as json/csv/parquet

When gen uses randomness, we have a monte carlo population

Results needs to pop out as combinable monoids.
That is easy enough for the flavor histograms.
For the congruence/similarity statistics, need to do some truncation, which may lead to non-deterministic results.

Note that numeric types of the tri-shape-descs are abstract:  they may be Rational, Int, Algebraic, or some Refined-type.
Various other constraints may apply to the DISTRIBUTION of the tri-shape-descs.

 Then the results from chunks of inputs

 What are limits on what we can do with regular collections, vs. with Spark, vs. with Fs2 stream?

 What if we have an explicit model of the TriShape distro?
All sides are ints between 1 and 5, so max 125 unique tri-shapes

 triShapes, histos, distros

Can build up scala-tests,
then perhaps incrementally extend towards prob-monads
as well as an axio-GUI view
and put meat on bones of axLam + YAFL

 So we can write down a math model and wire into any env to leverage proof-knowledge, etc.

We might use "the set of triangles such that ..." as a monte-carlo source, or as a symbolic entity.

Remember that triangles are 2-simplexes, which may be glued into algebraic complexes

(Tri)ShapeDesc is description of a geometric object using algebraically composed exact numbers.
"Composed" applies structurally (product-types and sum-types).
We also derive aggregatable shapeStats, which may include weights/counts, and which computationally compose with monoids.

ShapeStat[SD, KWrap] {
	shapeKey indicating some equivClz - monadic, closely tied to the outer type of ShapeStat, computable from an sd : SD.
		shapeKey : KWrap[SD]
	totalCount - always integer like
	totalPerim - same as SD.PerimType
	totalArea  - same as SD.AreaType

	combine(otherSS : ShapeStat[SD, KWrap])
}

ShapeStat is composed with an algebra like Spivak's Poly.

  Ring using |+| and |*| operations
  shstA |+| shstB = (disjoint) occurrence of A 'or' B (equiprobably)
  shstA |*| shstB = co-occurrence of some A & B (with some joint probability)

We may construct a shapeStat thru morphism from shapeDesc
  someMorphism : Function1[ShapeDesc,ShapeStat]

Because ShapeDesc is parametric in numeric types, ShapeStat winds up with same parameters.

ShapeStat represents a combinable observation of an occurrence
  monoidal operations are
 *monoidal* value, combinable with any ShapeStat of compatible type.
A shapeSt *may* be keyed.  Combination
A shapeSt *may* contain other shape stats (which *may* be keyed)
Map from shapeDesc


Want to try cats-free on the stat types
*/

trait ShapeStat[SD, KWrap[_]] {
	//shapeKey indicating some equivClz - monadic, closely tied to the outer type of ShapeStat, computable from an sd : SD.
	def shapeKey : KWrap[SD]
	def totalCount : Int //  - always integer like
}
trait TriShapeStat[TSD <: TriShape, KWrap[_]] {
	// def getPerim : TSD.LengthTy
	def perimFor(tsd : TSD) = tsd.perimeter
	def areaFor(tsd : TSD) = tsd.area
	def keyFor(tsd : TSD) : KWrap[TSD]
	// totalArea  - same as SD.AreaType
	// Monoidal so CANT-FAIL, so no-monad-needed
	def combine(otherTSS : TriShapeStat[TSD, KWrap]): TriShapeStat[TSD, KWrap] = {
		// val otherPerim = perimFor
		???
	}
}

/***
 * We may key the stats by any pure function of TSD.
 * 		First one is Isoceles, Right, ...
 */

trait FlavorTagKey[TSD]

trait TriOps[TSD <: TriShape] {
	val goodFC : FlavorCollector

	def tasteFlavor(tsd : TSD) : TSRFlavorTag = {
		???
	}
	case class EzFTK(tag : TSRFlavorTag) extends FlavorTagKey[TSD]

	def makeFlavorStat(tsd : TSD) : TriShapeStat[TSD, FlavorTagKey] = {
		val flavTag = tasteFlavor(tsd)
		val ftKey = new EzFTK(flavTag)
		val pureStat = new TriShapeStat[TSD, FlavorTagKey] {
			override def keyFor(tsd: TSD): FlavorTagKey[TSD] = ftKey
		}
		pureStat
	}
}

