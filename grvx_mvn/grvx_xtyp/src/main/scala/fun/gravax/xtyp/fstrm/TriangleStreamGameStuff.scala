package fun.gravax.xtyp.fstrm

import cats.{Applicative, Eval, Functor}
import cats.effect.{ExitCode, IO, IOApp}
import fun.gravax.xtyp.mathy.TriSideRec
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape}
import fs2.{Pipe, Pure, Stream}

import scala.util.Random

private trait TriangleStreamGameStuff

object RunTriStreamGame extends IOApp {

	val ourMGF = new MakesGameFeatures {}

	override def run(args: List[String]): IO[ExitCode] = {
		val helloJob: IO[Unit] = IO.println("RunTriStreamGame asks:  Who wants to play with Triangles?")
		val triJob = ourMGF.mkJobThatPrintsFewTris
		val manyJob = ourMGF.mkJobThatPrintsManyTris
		helloJob.productR(triJob).productR(manyJob).as(ExitCode.Success)
	}
	// (IO(println("started")) >> IO.never).onCancel(IO(println("canceled")))

}
trait MakesGameFeatures {
	val ourTsMkr = new TriStreamMaker {}
	def mkJobThatPrintsFewTris : IO[Unit] = {
		val firstSubJob = IO.apply {
			val tshp = ourTsMkr.makeTriShape
			val ttxt = triShapeToTxt(tshp)
			println(s"Dumped tshp=${tshp} as txt=${ttxt}")
			val randT = ourTsMkr.mkTriWithRandSidesForFixedPerim(20, 4)
			println(s"Dumped randT=${tshp} as txt=${triShapeToTxt(randT)}")
			val strmOf2: Stream[Pure, TriShape] = Stream.emits(List(tshp, randT))
			val strmOf30 = strmOf2.repeatN(15)
			val strmOf9 = strmOf30.takeRight(9)
			val outTxt = dumpFiniteStreamOfTrisIntoTxtBlock(strmOf9)
			println(s"9 tris:\n=================\n${outTxt}\n===================")

		}
		val job4vectFromStream: IO[Vector[Int]] = ourTsMkr.mkStreamOfRandomInts(3, 8).take(10).compile.toVector
		val jobChainSoFar = firstSubJob.productL(job4vectFromStream.flatMap(v => IO.println("Generated int vector-from-stream using MANY rngs: " + v)))
		val job4strm: IO[Stream[IO, Int]] = ourTsMkr.makeJobToProduceRngAndThenNumStream(7,39)
		val job4out: IO[Unit] = job4strm.flatMap(strm => {
			val outVectJob: IO[Vector[Int]] = strm.repeat.take(11).compile.toVector
			outVectJob.flatMap(v =>  IO.println("Generated int vector-from-stream using ONE rng: " + v))
		})
		val lastJobChain = jobChainSoFar.productL(job4out)
		lastJobChain
	}
	def dumpFiniteStreamOfTrisIntoTxtBlock(pureStrmOfTri : Stream[Pure, TriShape]) : String = {
		val trisWithIdx: Stream[Pure, (TriShape, Long)] = pureStrmOfTri.zipWithIndex
		val outLinesStrm: Stream[Pure, String] = trisWithIdx.map(tup => s"tri #=${tup._2} as txt=${triShapeToTxt(tup._1)}}")
		// FIXME: Combining Strings with + is wasteful
		val outBlockStrm: Stream[Pure, String] = outLinesStrm.reduce((s1, s2) => s1 + "\n" + s2)
		val outTxt = outBlockStrm.toList.toString()
		outTxt
	}
	// A Pipe is a function mapping one stream to another.
	// It can be applied using strm.through(pipe)
	def pipeToConsole[X]: Pipe[IO, X, Unit] = inStrm => {
		val inStrmIO_X : Stream[IO, X] = inStrm
		// evalMap(f) is alias for:   flatMap(o => Stream.eval(f(o))).
		val outStreamOfIO: Stream[IO, Unit] = inStrmIO_X.evalMap(x => {
			// We are building an effect that processes x
			val xOfX: X = x
			IO.println(xOfX)
		})
		outStreamOfIO
	}
	/*
	https://fs2.io/#/guide?id=building-streams

	The eval function works for any effect type, not just IO. FS2 does not care what effect type you use for your streams. You may use IO for effects or bring your own, just by implementing a few interfaces for your effect type (e.g., cats.MonadError[?, Throwable], cats.effect.Sync, cats.effect.Async, cats.effect.Concurrent). Here's the signature of eval:

	Any Stream formed using eval is called 'effectful' and can't be run using toList or toVector
	 */
	def mkPauseStream(msec : Long) : Stream[IO, Unit] = {
		val pauseStream: Stream[IO, Unit] = Stream.eval {
			IO {
				println(s"PAUSE EFFECT START at ${System.currentTimeMillis()}: Pausing with nasty Thread.sleep for ${msec}")
				Thread.sleep(msec)
				println(s"PAUSE EFFECT END at ${System.currentTimeMillis()}")
			}
		}
		pauseStream
		val attStrm: Stream[IO, Either[Throwable, Unit]] = pauseStream.attempt
/*
 several methods available to 'compile' the stream to a single effect:
Notice these all return a IO of some sort, but this process of compilation doesn't actually perform any of the effects
 (nothing gets printed).


val ra = eff.compile.toVector // gather all output into a Vector
// ra: IO[Vector[Int]] = IO(...) // gather all output into a Vector
val rb = eff.compile.drain // purely for effects
// rb: IO[Unit] = IO(...) // purely for effects
val rc = eff.compile.fold(0)(_ + _) // run and accumulate some result
// rc: IO[Int] = IO(...)
 */
		val cps: Stream.CompileOps[IO, IO, Unit] = pauseStream.compile
		// def other = Stream.eval { Pu}
		pauseStream
	}


	def printStreamUsingCatsStdCon(strm : Stream[Pure, _]): Unit = {
		// import cats.effect.std._
		// strm.printlns
	}
	def mkJobThatPrintsManyTris : IO[Unit] = {
		IO.apply {
			val someInts = ourTsMkr.streamSomeInts
			val outInts = someInts.toList
			println(s"Dumped someInts=${someInts} as outInts=${outInts}")
			val perimsRange = Stream.range(10, 40, 3)
			println(s"Rangey stream .toList = ${perimsRange.toList}")
			val pairly = perimsRange.flatMap(perim => Stream.range(1, 4).map(minLen => (perim, minLen)))
			println(s"Pairly stream .toList = ${pairly.toList}")
			val triStrm = pairly.map(pair => {
				ourTsMkr.mkTriWithRandSidesForFixedPerim(pair._1, pair._2)
			})
			val outTxt = dumpFiniteStreamOfTrisIntoTxtBlock(triStrm)
			println(s"many tris:\n=================\n${outTxt}\n===================")
		}
	}
	def triShapeToTxt(ts : TriShape) : String = {
		val sides = ts.sidesIncreasing
		val perim = ts.perimeter
		val area = ts.area
		s"[sides=${sides}, perim=${perim}, area=${area}]"
	}
}

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
	private val myTsxMaker = new MakesTSX {}
	def makeTriShape : TriShape = {
		val tsx345 = myTsxMaker.mkFromSidesIncreasing(3, 4, 5)
		tsx345
	}
	// TODO:  Treat the RNG as an effect.  Also integrate with Spire Rand, Distribution, etc.
	// TODO:  Allow user to set min/max bounds on lengths
	val myRndm = new Random()
	def mkTriWithRandSidesForFixedPerim(perim : Int, minSideLen : Int) : TriShape = {
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
	import cats.effect.std.{Random => CatsRandom}

	// Each time this job is run, it returns a reference to a good source of random numbers.
	// We don't know if it's the same instance of scalaUtilRandom, but as long as we are using
	// it within cats-effect we expect it to behave well, even under concurrency.
	// However if we run it millions of times, then we MIGHT have created millions of ScalaRandom instances?!
	// See cats-effect code snippets at bottom of this file
	val myRngMakerJob : IO[CatsRandom[IO]] = CatsRandom.scalaUtilRandom[IO]
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
	/*
	def shareOneRng(minIncl : Int, maxIncl : Int) : Stream[IO, Int] = {
		val range = maxIncl - minIncl + 1
		myRngMakerJob.flatMap(rng => {

		})
	}
	 */
	// Here we assume that a single rng is already made for us, which allows us to wire it directly into an effect.
	// How could we constrain F to ensure it has .map()?
	def makeRandomNumStream[F[_]](rng : CatsRandom[F], minIncl : Int, maxIncl : Int) : Stream[F, Int] = {
		val range = maxIncl - minIncl + 1
		val streamOne: Stream[F, Int] = Stream.eval {
			// We don't have any type info allowing us to call numJob.map
			val numJob: F[Int] = rng.nextIntBounded(range)  // . // map(_ + minIncl)
			numJob
		}
		// ...so instead we perform a .map out here at the stream level.
		streamOne.map(_ + minIncl) // Just one element in the stream
	}
	def makeJobToProduceRngAndThenNumStream(minIncl : Int, maxIncl : Int) : IO[Stream[IO, Int]] = {
		myRngMakerJob.map(rng => makeRandomNumStream(rng, minIncl, maxIncl))
	}


}

/*

The key operations are ++, map, flatMap, handleErrorWith, and bracket:



https://www.javadoc.io/doc/co.fs2/fs2-docs_2.13/3.5.0/fs2/Stream.html


 A Chunk is a strict, finite sequence of values that supports efficient indexed based lookup of elements.

 Regardless of how a Stream is built up, each operation takes constant time. So s ++ s2 takes constant time,
 regardless of whether s is Stream.emit(1) or it's a huge stream with millions of elements and lots of embedded effects.
 Likewise with s.flatMap(f) and handleErrorWith
 The runtime of these operations do not depend on the structure of s

 FS2 streams are chunked internally for performance. You can construct an individual stream chunk using Stream.chunk, which accepts an fs2.Chunk

 val s1c = Stream.chunk(Chunk.array(Array(1.0, 2.0, 3.0)))

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