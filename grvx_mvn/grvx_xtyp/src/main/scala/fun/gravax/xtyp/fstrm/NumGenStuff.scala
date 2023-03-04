package fun.gravax.xtyp.fstrm

import cats.{FlatMap, Functor}
import cats.effect.{IO, SyncIO}
import cats.effect.kernel.Sync
import fs2.{Pure, Stream}

import scala.util.{Random => ScRandom}
import cats.effect.std.{Random => CatsRandom}

private trait NumGenStuff

trait NumJobMaker {
	// These functions each allow a type paramter for effect type F.
	// This is unnecessarily general, since whatever context builds the NumJobMaker
	// will probably use the same Effect type.  So might as well use a trait or class level type parameter,
	// as we do in TriStreamMaker.  However, note that traits cannot use context bounds.

	// Here we constrain F to support .map, using   :Functor and implicitly
	// "Generally, a type parameter with a context bound is of the form [T: Bound]; it is expanded to plain type
	// parameter T together with an implicit parameter of type Bound[T]."
	// https://www.baeldung.com/scala/view-context-bounds
	// https://www.baeldung.com/scala/implicitly

	private val dbgImps : Boolean = false
	// The Functor context bound implies the existince of an implicit parameter.
	def makeRandomRangedNumEffect[F[_] : Functor](rng: CatsRandom[F], minIncl: Int, maxIncl: Int): F[Int] = {
		val range = maxIncl - minIncl + 1
		val rangedNumJob: F[Int] = rng.nextIntBounded(range)
		val impFunct: Functor[F] = implicitly[Functor[F]]
		if (dbgImps) println(s"makeRandomRangedNumEffect got implicit functor instance: ${impFunct}, minIncl=${minIncl}, maxIncl=${maxIncl}")
		val shiftedNumJob = impFunct.map(rangedNumJob)(num => num + minIncl)
		shiftedNumJob
	}

	def makeRangedNumJobUsingSyncRandom[F[_] : Sync](minIncl : Int, maxIncl : Int) : F[Int] = {
		// This works, but notice that our RNG-fetching is part of the job.  So when we repeat this job...
		val rngMakerJob: F[CatsRandom[F]] = CatsRandom.scalaUtilRandom[F] // requires implicit evidence : cats.effect.kernel.Sync[F]
		// Sync => Monad => FlatMap
		val impFM = implicitly[FlatMap[F]]
		if (dbgImps) println(s"makeRangedNumJobUsingSyncRandom got implicit FlatMapper instance: ${impFM}")
		impFM.flatMap(rngMakerJob)(rng => makeRandomRangedNumEffect(rng, minIncl, maxIncl))
	}

	// Here we assume that a single rng is already made for us, which allows us to wire it directly into an effect.
	// Functor constraint ensures that F supports map()
	def makeRandomNumStream[F[_] : Functor](rng: CatsRandom[F], minIncl: Int, maxIncl: Int): Stream[F, Int] = {
		val range = maxIncl - minIncl + 1
		val streamOne: Stream[F, Int] = Stream.eval {
			val numJob: F[Int] = rng.nextIntBounded(range)
			val impFunct = implicitly[Functor[F]]
			if (dbgImps) println(s"makeRandomNumStream got implicit functor instance: ${impFunct}, minIncl=${minIncl}, maxIncl=${maxIncl}")
			val shifted = impFunct.map(numJob)(_ + minIncl)
			shifted
		}
		// If we wanted to avoid the "implicitly" magic, we could perform a .map out here at the stream level.
		// streamOne.map(_ + minIncl)
		streamOne // Just one element in the stream, but we don't have a type to indicate that.
	}

}

trait RandosBoundToIO {
	type Eff[X] = IO[X] // How do we weaken this to say only that Eff must implement FlatMap?
	// A class-param context bound can do that.

	// Each time this job is run, it returns a reference to a good source of random numbers.
	// We don't know if it's the same instance of scalaUtilRandom, but as long as we are using
	// it within cats-effect we expect it to behave well, even under concurrency.
	// However if we run it millions of times, then we MIGHT have created millions of ScalaRandom instances?!
	// In source for v~3.3 looks like it does create a new one on each access to scalaUtilRandom, but scalaUtilRandomN is
	// mmore subtle.  See cats-effect code snippets at bottom of this file
	// TODO:  Try out .scalaUtilRandomSeedInt/Long and  .scalaUtilRandomN
	val myRngMakerJob : Eff[CatsRandom[Eff]] = CatsRandom.scalaUtilRandom[Eff]

	// val myRngMakerEval = CatsRandom.scalaUtilRandom[Eval]
	def makeRngJob(minIncl : Int, maxIncl : Int) = {
		val range = maxIncl - minIncl + 1
		// job is responsible for both building an rng AND then generating the value.
		// So if we run this job 1000 times, we have created 1000 rngs, which is not what we want.
		val job: Eff[Int] = myRngMakerJob.flatMap(rng => {
			rng.nextIntBounded(range).map(_ + minIncl)
		})
	}
	def mkStreamOfRandomInts(minIncl : Int, maxIncl : Int) : Stream[Eff, Int] = {
		val range = maxIncl - minIncl + 1
		val streamOne: Stream[Eff, Int] = Stream.eval {

			val compoundJobHereIsJustAsBad = myRngMakerJob.flatMap(rng => {
				val x: CatsRandom[Eff] = rng
				val jobToMakeNum: Eff[Int] = rng.nextIntBounded(range).map(_ + minIncl)
				jobToMakeNum
			})
			compoundJobHereIsJustAsBad
		}
		streamOne.repeat
	}
	val myNJM = new NumJobMaker{}
	def makeJobToProduceRngAndThenNumStream(minIncl : Int, maxIncl : Int) : Eff[Stream[Eff, Int]] = {
		myRngMakerJob.map(rng => myNJM.makeRandomNumStream(rng, minIncl, maxIncl))
	}

}
trait OtherNums {
	//  def unfold[F[x] >: fs2.Pure[x], S, O](s : S)(f : scala.Function1[S, scala.Option[scala.Tuple2[O, S]]]) : fs2.Stream[F, O] = { /* compiled code */ }
	def streamNumsUsingUnfold: Stream[Pure, Int] = {
		val initState: String = "wow"
		val strm = Stream.unfold(initState)(st => {
			val stLen = st.length
			if (stLen > 25) None else {
				val nextState = st + (-1 * stLen).toString
				val output: Int = stLen * 100
				Some((output, nextState))
			}
		})
		strm
	}
}