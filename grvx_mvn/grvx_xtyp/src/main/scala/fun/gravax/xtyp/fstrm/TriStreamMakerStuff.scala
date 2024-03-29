package fun.gravax.xtyp.fstrm

import cats.{Applicative, Eval, FlatMap, Functor}
import cats.effect.IO
import cats.effect.kernel.Sync
import fs2.{Pure, Stream}
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape, TriShapeXactish}

import scala.util.{Try, Random => ScRandom}
import cats.effect.std.{Random => CatsRandom}

private trait TriStreamMakerStuff

/*
	// Stages of tri gen
	// Generate parameters, e.g. perimeter & minLen.  May be done randomly or determ.
		// These params may be supplied as a stream of 2-tuples.
	// Generate side-Lengths and order them.  Usually requires 2 randoms.
		// Could be a stream of 3-tuples.
	// Build the actual triangle, which may fail with exception due to triangle-inequality.
		// Does not require any additional random nums.

 */
// traits cannot have type parameters with context bounds `: ...` nor view bounds `<% ...`
trait TriStreamMaker[Eff[_] ] { 	// User must bind an effect type when instantiating the trait.
	def getFM : FlatMap[Eff]	// We need this instance a lot.  We pass it explicitly.
	def getSync : Sync[Eff]		// We need this only when we are making an RNG
	val myNJM = new NumJobMaker{}
	private val flg_dbgImps : Boolean = false
	// val impSync = getSync
	implicit val impFMF: FlatMap[Eff] = getFM // This val is used by the calls below to .makeRandomRangedNumEffect


	// Produces a job which generates ONE triple of partially constrained side lengths (which *may* form a legal
	// triangle) each time it is run.
	// TODO:  Produce a more general version that can use different numeric types (from Spire).
	// Note this will be type-variance in the DOMAIN-MODELING dimension rather than the PLATFORM-TECH dimension (Eff).
	// When we are fully polymorphic in both, then we are closer to capturing the essence of the domain-functional
	// idea (generate 3 random-ish side-lengths which *may* form a legal triangle).
	// Question:  How much faster is it to produce a chunk of N of these triples in one go, rather than to run this
	// job N times?

	// Elements of the computation:
	// Constraints:  perim, minSideLen
	// Impure data-source:  rng
	// Output triple of numbers
	// NUM type
	// EFF type
	// Algorithm of generation, with implied biases.  This algo implies a distribution for the side-triples.

	// TODO:  Produce a version that can generate using SAS = Side-Angle-Side, which can give us one intuitive
	// kind of uniformity in the distribution of triangles, avoiding illegal triangles.  The tradeoff is in the
	// gnarliness of dealing with angles.  Sensible constraints could be just minSideLen and maxSideLen, although
	// we could also constrain the angles.
	// This alternate algo provides variation in the DOMAIN-MODELING dimension.

	def makeTriSidesJob(rng: CatsRandom[Eff], perim: Int, minSideLen: Int): Eff[(Int, Int, Int)] = {
		// Same algo used in NaiveTriMaker, but all randomness is suspended in the Eff-ect.
		// Note that we use the rng TWICE, and the param for the second is dependent on result from the first.
		assert(perim >= 3 * minSideLen)
		val maxSideLen = perim - 2 * minSideLen

		// If we don't pass impFMF, the code compiles but makeRandomRangedNumEffect can't see the value and
		// it winds up using a NULL functor value.  Gross!
		// Hmm, even when it is passed....
		if (flg_dbgImps) println(s"makeTriSidesJob: impFMF=${impFMF}")
		val sideLenX_job: Eff[Int] = myNJM.makeRandomRangedNumEffect(rng, minSideLen, maxSideLen)
		// Now that we have a job to make sideLenX, we chain with flatMap into a dependent generation of sideLenY.
		// TODO: Consider other ways we can generate and constrain the distribution of the tri-sides.
		val sidesTupleJob: Eff[(Int, Int, Int)] = impFMF.flatMap(sideLenX_job)(sideLenX => {
			val maxY = perim - sideLenX - minSideLen

			// This time we choose to pass impFMF in explicitly.
			val sideLenY_job: Eff[Int] = myNJM.makeRandomRangedNumEffect(rng, minSideLen, maxY)(impFMF)
			impFMF.map(sideLenY_job)(sideLenY => {
				// Now that we have sideLenX and sideLenY, we can compute sideLenZ as the remainder (of perimeter).
				val sideLenZ = perim - sideLenX - sideLenY
				assert((sideLenX + sideLenY + sideLenZ) == perim) // sanity check
				// Now we want to put the 3 sides in increaing order.
				// FIXME:  Do this ordering more elegantly
				val lame: Seq[Int] = Vector(sideLenX, sideLenY, sideLenZ).sorted
				val sidesTupleRslt = lame match {
					case Seq(a, b, c) => (a, b, c) // Can we find an easier way to make a tuple from a Seq?
				}
				sidesTupleRslt
			})
		})
		sidesTupleJob
	}
	// TODO: Make this shorter using .through
	def makeTriSidesStreamUsingEvalMap(rng: CatsRandom[Eff], pairStrm: Stream[Eff, (Int, Int)]): Stream[Eff, (Int, Int, Int)] = {

		// Generate exactly one ordered-side-triple for each input param-pair.
		// def evalMap[F2[x] >: F[x], O2](f: (O) => F2[O2]): Stream[F2, O2]
		// Alias for flatMap(o => Stream.eval(f(o))).
		pairStrm.evalMap(paramPair => {
			if (false) println(s"makeTriSidesStreamUsingEvalMap:  rng=${rng}, paramPair=${paramPair}")
			makeTriSidesJob(rng, paramPair._1, paramPair._2)
		})
	}

	private val myTsxMaker = new MakesTSX {}
	type TriErrMsg = String
	type TriGenRslt = Either[TriErrMsg, TriShapeXactish]
	// Should mkXactTriOrErr build an effect, or claim to be pure?
	// A function which catches an exception might be deemed less than 100% pure.
	// If we pre-checked the tri-ineq, then we would have a pure and fast failure, and exceptions would never happen.
	// This is a function we expect to execute a lot.
	// We might sometimes execute it against pure inputs.  Doing 'catch' implies some performance penalty.
	def mkXactTriJob(orderedSidesTup : (Int, Int, Int)) : Eff[TriGenRslt] = {
		val (a, b, c) = orderedSidesTup
		// mkFromSidesIncreasing may throw
		// If we were committed to IO affect we could: IO.apply(myTsxMaker.mkFromSidesIncreasing(a, b, c))
		// But using IO directly in this kind of domain-oriented compute is questionable as a practice.
		// If we know an instance of some helper/companion for Eff, such as Sync[Eff], then we can lift the compute with that.
		// At that point we are implying all the laws of Sync[Eff], but in a way that is a bit wobbly from proof standpoint.
		val syncEff = getSync
		// "If your side effect is not thread-blocking then you can use Sync[F].delay"
		var bareTriEff: Eff[TriShapeXactish] = syncEff.delay(myTsxMaker.mkFromSidesIncreasing(a, b, c))
		// Now to handle the exception we use
		val redeemedToEith: Eff[Either[TriErrMsg, TriShapeXactish]] = syncEff.redeem(bareTriEff)(thrn => Left(thrn.toString), tri => Right(tri))
		// With attempt we can immediately get the Either, but note that to map we again, we have to pull in an instance,
		// because the Eff type has no methods.
		val bareAttempted: Eff[Either[Throwable, TriShapeXactish]] = syncEff.attempt(bareTriEff)

		// bareTriEff.redeem[Either[TriErrMsg, TriShapeXactish]](thrn => Left(thrn.toString), tri => Right(tri))
		redeemedToEith
	}
	def mkExactTriSemipure(orderedSidesTup : (Int, Int, Int)) : TriGenRslt = {
		val (a, b, c) = orderedSidesTup
		val triTry = Try(myTsxMaker.mkFromSidesIncreasing(a, b, c))
		val eithThrw: Either[Throwable, TriShapeXactish] = triTry.toEither
		eithThrw.left.map(thrn => thrn.toString)
	}
	def makeTriSidesStreamUsingThrough(rng: CatsRandom[Eff], pairStrm: Stream[Eff, (Int, Int)]): Stream[Eff, (Int, Int, Int)] = {
		//	wrong = pairStrm.through
		???
	}
	// PureRandomized => Random-Effects were already run, to capture a stream of random ints into memory.
	// But this is not really any better than an eagerly produced List of Tris.
	def getFinitePureRandomizedTris(cnt: Int, perim: Int, minSideLen: Int): Stream[Pure, TriShape] = {
		// TODO:  Use .handleErrorWith / .attempt to map exceptions caused by triangle-inequality.
		???
	}
}

class ClzWithContextBound[Eff[_] : Sync] { //
	val impFM = implicitly[FlatMap[Eff]]

	def processUsingCtx(inJob : Eff[String]): Unit = {

	}
}


trait NaiveTriMaker {
	private val myTsxMaker = new MakesTSX {}

	// Naive RNG is used directly.  There is no suspending of effect.
	private val myRndm = new ScRandom()


	def nowMkTriWithRandSidesForFixedPerim(perim: Int, minSideLen: Int): TriShape = {
		// This algo does not guarantee a legal triangle.  But we make sure that 3 individually legal values are generated.

		// By using RNG directly, we are able to compute immediately with values, instead of chaining.
		// If we had access to a pure stream of random ints, we could pull values from it, but note we would
		// also need to return the amount of ints we used up (or the tail of the RNG stream), and the stream would
		// usually need to be treated as a private pool of ints per concurrent task.


		assert(perim >= 3 * minSideLen)
		val maxSideLen = perim - 2 * minSideLen
		val rangeX = maxSideLen - minSideLen
		val sideLenX = minSideLen + myRndm.nextInt(rangeX)
		val rangeY = perim - sideLenX - minSideLen
		val sideLenY = minSideLen + myRndm.nextInt(rangeY)
		val sideLenZ = perim - sideLenX - sideLenY
		assert((sideLenX + sideLenY + sideLenZ) == perim)
		val lame: Seq[Int] = Vector(sideLenX, sideLenY, sideLenZ).sorted
		val tsx = lame match {
			case Seq(a, b, c) => myTsxMaker.mkFromSidesIncreasing(a, b, c)	// may throw
		}
		tsx
	}

	def nowMakeOneRightTriShape(scale : Int) : TriShape = {
		assert (scale >= 1)
		val tsx345 = myTsxMaker.mkFromSidesIncreasing(scale * 3, scale * 4, scale * 5)
		tsx345
	}


}

/*

   * Creates a new random number generator
  def scalaUtilRandom[F[_]: Sync]: F[Random[F]] =
    Sync[F].delay {
      val sRandom = new SRandom()
      new ScalaRandom[F](sRandom.pure[F]) {}

   * Creates a new random number generator using a single integer seed.
  def scalaUtilRandomSeedInt[F[_]: Sync](seed: Int): F[Random[F]] =
    Sync[F].delay {
      val sRandom = new SRandom(seed)
      new ScalaRandom[F](sRandom.pure[F]) {}
    }
   * Creates Several Random Number Generators and equally allocates the load across those
   * instances.
   *
   * From the java class docs:
   * https://docs.oracle.com/javase/8/docs/api/java/util/Random.html#java.util.Random
   *
   * Instances of java.util.Random are threadsafe. However, the concurrent use of the same
   * java.util.Random instance across threads may encounter contention and consequent poor
   * performance. Consider instead using ThreadLocalRandom in multithreaded designs.

  def scalaUtilRandomN[F[_]: Sync](n: Int): F[Random[F]] =
    for {
      ref <- Ref[F].of(0)
      array <- Sync[F].delay(Array.fill(n)(new SRandom()))
    } yield {
      def incrGet = ref.modify(i => (if (i < (n - 1)) i + 1 else 0, i))
      def selectRandom = incrGet.map(array(_))
      new ScalaRandom[F](selectRandom) {}
    }


def eval[F[_], O](fo: F[O]): Stream[F, O]
Creates a single element stream that gets its value by evaluating the supplied effect.
If the effect fails, the returned stream fails.
Use attemptEval instead if a failure while evaluating the effect should be emitted as a value.

def attemptEval[F[_], O](fo: F[O]): Stream[F, Either[Throwable, O]]
Creates a single element stream that gets its value by evaluating the supplied effect.
If the effect fails, a Left is emitted. Otherwise, a Right is emitted.
 */