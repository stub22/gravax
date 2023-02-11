package fun.gravax.xtyp.fstrm

import cats.effect.IO
import fs2.{Pure, Stream}
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape}

import scala.util.Random

private trait TriStreamMakerStuff

trait TriStreamMaker {

	//  def unfold[F[x] >: fs2.Pure[x], S, O](s : S)(f : scala.Function1[S, scala.Option[scala.Tuple2[O, S]]]) : fs2.Stream[F, O] = { /* compiled code */ }
	def streamSomeInts : Stream[Pure, Int] = {
		val initState : String = "wow"
		val strm = Stream.unfold(initState)(st => {
			val stLen = st.length
			if (stLen > 25) None else {
				val nextState = st + (-1 * stLen).toString
				val output : Int = stLen * 100
				Some((output, nextState))
			}
		})
		strm
	}
	// FIXME:  The randomness makes it not really Pure.  Also the possibility of exception is impure-ish
	def streamRandomTris(cnt_opt : Option[Int], perim : Int, minSideLen : Int) : Stream[Pure, TriShape] = {
		// TODO:  Use .handleErrorWith / .attempt to map exceptions caused by triangle-inequality.
		???
	}


	import cats.effect.std.{Random => CatsRandom}
	// Each time this job is run, it returns a reference to a good source of random numbers.
	// We don't know if it's the same instance of scalaUtilRandom, but as long as we are using
	// it within cats-effect we expect it to behave well, even under concurrency.
	// However if we run it millions of times, then we MIGHT have created millions of ScalaRandom instances?!
	// In source for v~3.3 looks like it does create a new one on each access to scalaUtilRandom, but scalaUtilRandomN is
	// mmore subtle.  See cats-effect code snippets at bottom of this file
	// TODO:  Try out .scalaUtilRandomSeedInt/Long and  .scalaUtilRandomN
	val myRngMakerJob : IO[CatsRandom[IO]] = CatsRandom.scalaUtilRandom[IO]


	def mkTriProducer[F[_]](rng : CatsRandom[F], perim : Int, minSideLen : Int): F[TriShape] = {
		???
	}

	// val myRngMakerEval = CatsRandom.scalaUtilRandom[Eval]
	def makeRngJob(minIncl : Int, maxIncl : Int) = {
		val range = maxIncl - minIncl + 1
		// job is responsible for both building an rng AND then generating the value.
		// So if we run this job 1000 times, we have created 1000 rngs, which is not what we want.
		val job: IO[Int] = myRngMakerJob.flatMap(rng => {
			rng.nextIntBounded(range).map(_ + minIncl)
		})
	}
	def streamOfRands : Unit = {
		//
	}
	def mkStreamOfRandomInts(minIncl : Int, maxIncl : Int) : Stream[IO, Int] = {
		val range = maxIncl - minIncl + 1
		val streamOne: Stream[IO, Int] = Stream.eval {

			val compoundJobHereIsJustAsBad = myRngMakerJob.flatMap(rng => {
				val x: CatsRandom[IO] = rng
				val jobToMakeNum: IO[Int] = rng.nextIntBounded(range).map(_ + minIncl)
				jobToMakeNum
			})
			compoundJobHereIsJustAsBad
		}
		streamOne.repeat
	}

	// Here we assume that a single rng is already made for us, which allows us to wire it directly into an effect.
	// How could we constrain F to ensure it has .map()?
	def makeRandomNumStream[F[_]](rng : CatsRandom[F], minIncl : Int, maxIncl : Int) : Stream[F, Int] = {
		val range = maxIncl - minIncl + 1
		val streamOne: Stream[F, Int] = Stream.eval {
			// We don't have any type info allowing us to call numJob.map
			val numJob: F[Int] = rng.nextIntBounded(range)  // . // map(_ + minIncl)
			numJob
		}
		// ...and instead we perform a .map out here at the stream level.
		streamOne.map(_ + minIncl) // Just one element in the stream
	}
	def makeJobToProduceRngAndThenNumStream(minIncl : Int, maxIncl : Int) : IO[Stream[IO, Int]] = {
		myRngMakerJob.map(rng => makeRandomNumStream(rng, minIncl, maxIncl))
	}

}
trait NaiveTriMaker {
	private val myTsxMaker = new MakesTSX {}

	// Naive RNG is used directly.  There is no suspending of effect.
	private val myRndm = new Random()


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
			case Seq(a, b, c) => myTsxMaker.mkFromSidesIncreasing(a, b, c)
		}
		tsx
	}

	def nowMakeOneDummyTriShape : TriShape = {
		val tsx345 = myTsxMaker.mkFromSidesIncreasing(3, 4, 5)
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
 */