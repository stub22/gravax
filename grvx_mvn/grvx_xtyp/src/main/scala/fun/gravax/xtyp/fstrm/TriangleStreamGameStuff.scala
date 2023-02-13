package fun.gravax.xtyp.fstrm

import cats.effect.kernel.Sync
import cats.FlatMap
import cats.effect.{ExitCode, IO, IOApp}
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape, TriShapeXactish}
import fs2.{Pipe, Pure, Stream}

import scala.util.Try


private trait TriangleStreamGameStuff

object RunTriStreamGame extends IOApp {

	val ourMGF = new MakesGameFeatures {}

	override def run(args: List[String]): IO[ExitCode] = {
		val helloJob: IO[Unit] = IO.println("RunTriStreamGame/run/helloJob asks:  Who wants to play with Triangles?")
		val triJob = ourMGF.mkJobThatPrintsFewTris
		val manyJob = ourMGF.mkJobThatPrintsManyTris
		helloJob.productR(triJob).productR(manyJob).as(ExitCode.Success)
	}
	// (IO(println("started")) >> IO.never).onCancel(IO(println("canceled")))

}
trait MakesGameFeatures {
	val ourTsMkr = new TriStreamMaker[IO] {
		override def getFM: FlatMap[IO] = IO.asyncForIO  // Uh, this "works"...
		override def getSync: Sync[IO] = Sync[IO]
	}
	val myNaiveTriMaker = new NaiveTriMaker {}
	val myRandosForIO = new RandosBoundToIO{}
	val myTryConsumer = new TriStreamConsumer {}
	val myNJM = new NumJobMaker{}
	val myOtherNums = new OtherNums {}
	def mkJobThatPrintsFewTris: IO[Unit] = {
		val firstSubJob = IO.apply {
			val firstTri = myNaiveTriMaker.nowMakeOneRightTriShape(1)
			val ttxt = myTryConsumer.triShapeToTxt(firstTri)
			println(s"Dumped firstTri=${firstTri} as txt=${ttxt}")
			val randTT = Try(myNaiveTriMaker.nowMkTriWithRandSidesForFixedPerim(20, 4))
			println(s"Dumped randTT=${randTT}")
			val secondTri = randTT.getOrElse(myNaiveTriMaker.nowMakeOneRightTriShape(7))
			val strmOf2: Stream[Pure, TriShape] = Stream.emits(List(firstTri, secondTri))
			val strmOf30 = strmOf2.repeatN(15)
			val strmOf5 = strmOf30.takeRight(5)
			val outTxt = myTryConsumer.dumpFinitePureStreamOfTrisIntoTxtBlock(strmOf5)
			println(s"5 tris:\n=================\n${outTxt}\n===================")
			val summStrm: Stream[Pure, TriSetStat] = myTryConsumer.summarizeTriStream(strmOf5)
			val summOut = summStrm.toList
			println(s"Summarized 5 tris as: ${summOut}")

		}
		val job4vectFromStream: IO[Vector[Int]] = myRandosForIO.mkStreamOfRandomInts(3, 8).take(10).compile.toVector
		val jobChainSoFar = firstSubJob.productL(job4vectFromStream.flatMap(v => IO.println("Generated int vector-from-stream using MANY rngs: " + v)))
		val job4strm: IO[Stream[IO, Int]] = myRandosForIO.makeJobToProduceRngAndThenNumStream(7, 39)
		val job4out: IO[Unit] = job4strm.flatMap(strm => {
			val outVectJob: IO[Vector[Int]] = strm.repeat.take(11).compile.toVector
			outVectJob.flatMap(v => IO.println("Generated int vector-from-stream using ONE rng: " + v))
		})
		val bigOlJobChain = jobChainSoFar.productL(job4out)
		val oneMore = myNJM.makeRangedNumJobUsingSyncRandom[IO](50, 60)
		val omd: IO[Unit] = oneMore.flatMap(num => IO.println(s"'oneMore' job from makeRangedNumJobUsingSyncRandom produced num: ${num}"))
		bigOlJobChain *> omd
	}

	def dbgNow(dbgHead: String, dbgBody: String): Unit = println(dbgHead, dbgBody)

	def mkJobThatPrintsManyTris: IO[Unit] = {
		val dbgHead = "mkJobThatPrintsManyTris"
		val pairStrmJob: IO[Stream[Pure, (Int, Int)]] = IO.apply {

			val someInts = myOtherNums.streamNumsUsingUnfold
			val outInts = someInts.toList
			dbgNow(dbgHead, s"Dumped someInts=${someInts} as outInts=${outInts}")
			val perimsRange = Stream.range(10, 40, 3)
			dbgNow(dbgHead, s"Rangey stream .toList = ${perimsRange.toList}")
			val pairly: Stream[Pure, (Int, Int)] = perimsRange.flatMap(perim =>
					Stream.range(1, 4).map(minLen => (perim, minLen)))
			dbgNow(dbgHead, s"Pairly stream .toList = ${pairly.toList}")
			val naiveTriStrm = pairly.map(pair => {
				myNaiveTriMaker.nowMkTriWithRandSidesForFixedPerim(pair._1, pair._2)
			})

			val outTxt: Try[String] = Try(myTryConsumer.dumpFinitePureStreamOfTrisIntoTxtBlock(naiveTriStrm))
			dbgNow(dbgHead, s"NAIVE tris:\n=================\n${outTxt}\n===================")
			pairly
		}

		val sidesStreamJob: IO[Stream[IO, (Int, Int, Int)]] = makeSidesTupleStreamJob(pairStrmJob)

		val sideStrmDebugCompiledJob = sidesStreamJob.map(_.debug().compile) // type= IO[Stream.CompileOps[IO, IO, (Int, Int, Int)]]
		val sideStrmLstDumpJob: IO[List[(Int, Int, Int)]] = sideStrmDebugCompiledJob.flatMap(compStrm => {
			val sidesTupleListEff: IO[List[(Int, Int, Int)]] = compStrm.toList
			// FlatTap inserts an effect without changing the output
			val tappedEff: IO[List[(Int, Int, Int)]] = sidesTupleListEff.flatTap(sidesTupleList => {
				IO.println(s"${dbgHead}.tappedEff got SidesTuple-list: ${sidesTupleList}")
			})
			tappedEff // Will execute our tapped-print, and then produce the List
		})

		// Although it is cumbersome to initialize all these 'job' vals, the payoff is that we can later choose which
		// of them actually get run, without having to disable the setup code.
		val sidesEff: IO[Unit] = sidesStreamJob.flatMap(strm => {
			val dbgStrm = strm.debug()
			val listJob: IO[List[(Int, Int, Int)]] = dbgStrm.compile.toList
			val listDumpJob = listJob.flatMap(lst => IO.println(s"Side-Stream-list: ${lst}"))
			listDumpJob
		})

		val triEithStrmJob: IO[Stream[IO, Either[ourTsMkr.TriErrMsg, TriShapeXactish]]] = sidesStreamJob.map(stupStrm => {
			stupStrm.evalMap(sidesTup => ourTsMkr.mkXactTriJob(sidesTup))
		})
		val triWinStrmJob: IO[Stream[IO, TriShapeXactish]] = triEithStrmJob.map(triStrm => {
			// We drop all the errors and keep the TriShapes
			triStrm.debug().flatMap(eith => {
				val errOrTri: Either[ourTsMkr.TriErrMsg, TriShapeXactish] = eith
				Stream.fromOption(eith.toOption)
			})
		})
		val triSummStrmJob: IO[Stream[IO, TriSetStat]] = triWinStrmJob.map(triStrm => myTryConsumer.summarizeTriStream(triStrm))
		val triSummEff: IO[Unit] = triSummStrmJob.flatMap(triSummStrm => {
			val outSummJob: IO[List[TriSetStat]] = triSummStrm.compile.toList
			outSummJob.flatMap(summLst => IO.println(s"${dbgHead} Tri-Summ-list: ${summLst}"))
		})
		// Cats effect 3.3 does not have "IO.andWait"
		sidesEff >> triSummEff
		// is >> exactly the same as productR?
		// >> [B](that: => IO[B]): IO[B]
		//Runs the current IO, then runs the parameter, keeping its result. The result of the first action is ignored.
		// If the source fails, the other action won't run. Evaluation of the parameter is done lazily, making this
		// suitable for recursion.
	}

	def makeSidesTupleStreamJob(pairStreamJob: IO[Stream[Pure, (Int, Int)]]): IO[Stream[IO, (Int, Int, Int)]] = {
		import cats.effect.std.{Random => CatsRandom}
		val rngMakerJob: IO[CatsRandom[IO]] = CatsRandom.scalaUtilRandom[IO]
		val bothPrecursors: IO[(Stream[Pure, (Int, Int)], CatsRandom[IO])] = pairStreamJob.both(rngMakerJob)
		val sidesTupleStreamJob: IO[Stream[IO, (Int, Int, Int)]] = bothPrecursors.map(precPair => {
			val (pairStrm, rng) = precPair
			ourTsMkr.makeTriSidesStreamUsingEvalMap(rng, pairStrm.covary[IO])	// uses Stream.PureOps.covary
		})
		sidesTupleStreamJob
	}
}


/*

The key operations are ++, map, flatMap, handleErrorWith, and bracket:

scala> import cats.effect.SyncIO
scala> Stream(1, 2, 3).append(Stream.raiseError[SyncIO](new RuntimeException)).handleErrorWith(_ => Stream(0)).compile.toList.unsafeRunSync()
res0: List[Int] = List(1, 2, 3, 0)

https://www.javadoc.io/doc/co.fs2/fs2-docs_2.13/3.5.0/fs2/Stream.html


 A Chunk is a strict, finite sequence of values that supports efficient indexed based lookup of elements.

 Regardless of how a Stream is built up, each operation takes constant time. So s ++ s2 takes constant time,
 regardless of whether s is Stream.emit(1) or it's a huge stream with millions of elements and lots of embedded effects.
 Likewise with s.flatMap(f) and handleErrorWith
 The runtime of these operations do not depend on the structure of s

 FS2 streams are chunked internally for performance. You can construct an individual stream chunk using Stream.chunk, which accepts an fs2.Chunk

 val s1c = Stream.chunk(Chunk.array(Array(1.0, 2.0, 3.0)))


.prefetch(n).parEvalMapUnordered(n)

	 StringBuilder is faster than StringBuffer, because it is not threadsafe/synchronized.

IO combinators:

   * Like [[*>]], but keeps the result of the source.
   * For a similar method that also runs the parameter in case of failure or interruption, see
   * [[guarantee]].

  def <*[B](that: IO[B]): IO[A] =  productL(that)

   * Runs the current IO, then runs the parameter, keeping its result. The result of the first
   * action is ignored. If the source fails, the other action won't run. Not suitable for use
   * when the parameter is a recursive reference to the current expression.

  def *>[B](that: IO[B]): IO[B] = productR(that)

   * Runs the current IO, then runs the parameter, keeping its result. The result of the first
   * action is ignored. If the source fails, the other action won't run. Evaluation of the
   * parameter is done lazily, making this suitable for recursion.

  def >>[B](that: => IO[B]): IO[B] = flatMap(_ => that)

  def !>[B](that: IO[B]): IO[B] =   forceR(that)

   * Runs this IO and the parameter in parallel.
   * Failure in either of the IOs will cancel the other one. If the whole computation is
   * canceled, both actions are also canceled.
  def &>[B](that: IO[B]): IO[B] =   both(that).map { case (_, b) => b }

   * Like [[&>]], but keeps the result of the source
  def <&[B](that: IO[B]): IO[A] =  both(that).map { case (a, _) => a }

 */