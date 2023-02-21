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
		ourMGF.useCBoundClz
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

	val myPieceMaker = new TriStreamPieceMaker{}
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
			val pairStrm = myPieceMaker.mkPureStreamOfPairs
			myPieceMaker.dumpSmallSampleOfTris(pairStrm)
			pairStrm
		}

		val sidesStreamJob: IO[Stream[IO, (Int, Int, Int)]] = makeSidesTupleStreamJob(pairStrmJob)

		val sideStrmLstDumpJob: IO[List[(Int, Int, Int)]] = sidesStreamJob.flatMap(tstrm => {
			myPieceMaker.dumpSomeTrisToList(tstrm)
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

		// Unused fork showing how we could keep counts for Left+Right using zipWithScan
		val triEithWithCntsJob: IO[Stream[IO, (Either[ourTsMkr.TriErrMsg, TriShapeXactish], (Int, Int))]] =
				triEithStrmJob.map(eithStrm => eithStrm.zipWithScan1((0,0))((cntPair, eot) => {
			val leftIncr = if (eot.isLeft) 1 else 0
			val rightIncr = if (eot.isRight) 1 else 0
			(cntPair._1 + leftIncr, cntPair._2 + rightIncr)
		}))
		val triEithCntOutEff = triEithWithCntsJob.flatMap(eithPlusCntsStrm => {
			myPieceMaker.sampleTriResultsWithCnts(eithPlusCntsStrm)
		})

		// val failedCountStrmJob = triEithStrmJob.flatTap(triEithStrm => )
		// // , IO[(Int, Int)]
		val triWinStrmJob: IO[Stream[IO, TriShapeXactish]] = triEithStrmJob.map(triEithStrm => {
			// We drop all the errors and keep the TriShapes.  Easiest way to expose number of failures?
			val mightDebug = if (false) triEithStrm.debug() else triEithStrm
			val triWinStrm: Stream[IO, TriShapeXactish] = mightDebug.flatMap(eith => {
				val errOrTri: Either[ourTsMkr.TriErrMsg, TriShapeXactish] = eith
				Stream.fromOption(eith.toOption)
			})
			triWinStrm
		})

		val triSummStrmJob = triWinStrmJob.map(triWinStrm => myTryConsumer.summarizeTriStream(triWinStrm))
		val triSummEff: IO[Unit] = triSummStrmJob.flatMap(triSummStrm => {
			val outSummJob: IO[List[TriSetStat]] = triSummStrm.compile.toList
			outSummJob.flatMap(summLst => IO.println(s"${dbgHead} Tri-Summ-list: ${summLst}"))
		})

		// Cats effect 3.3 does not have "IO.andWait"
		val summThenCnts = triSummEff >> triEithCntOutEff
		val toDoChain = if(false) sidesEff >> summThenCnts else summThenCnts
		toDoChain
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
	def otherCountingJob(eithStrm : Stream[IO, Either[ourTsMkr.TriErrMsg, TriShapeXactish]]) : Unit = {
		val initCntPair = (0, 0)
		val countStreamOf1: Stream[IO, (Int, Int)] = eithStrm.fold(initCntPair)((cntPair, eot) => {
			val leftIncr = if (eot.isLeft) 1 else 0
			val rightIncr = if (eot.isRight) 1 else 0
			(cntPair._1 + leftIncr, cntPair._2 + rightIncr)
		})
		val cntOutPairJob: IO[(Int, Int)] = countStreamOf1.compile.toList.map(_.head)
	}

	def useCBoundClz : IO[Unit] = {
		val x= new ClzWithContextBound[IO]
		val preJob = IO.println("useCBoundClz/preJob says Hi!")
		// val outJob = x.processUsingCtx(preJob)
		preJob
	}
}
trait TriStreamPieceMaker {
	val myNaiveTriMaker = new NaiveTriMaker {}
	val myRandosForIO = new RandosBoundToIO{}
	val myTryConsumer = new TriStreamConsumer {}
	val myNJM = new NumJobMaker{}
	val myOtherNums = new OtherNums {}

	def dbgNow(dbgHead: String, dbgBody: String): Unit = println(dbgHead, dbgBody)


	def mkPureStreamOfPairs : Stream[Pure, (Int, Int)] = {
		val dbgHead = "mkPureStreamOfPairs"
		val someInts = myOtherNums.streamNumsUsingUnfold
		val outInts = someInts.toList
		dbgNow(dbgHead, s"Dumped someInts=${someInts} as outInts=${outInts}")
		val perimsRange = Stream.range(10, 4000, 3)
		dbgNow(dbgHead, s"Rangey stream sample .toList = ${perimsRange.take(10).toList}")
		val pairly: Stream[Pure, (Int, Int)] = perimsRange.flatMap(perim =>
			Stream.range(1, perim / 5).map(minLen => (perim, minLen)))

		pairly
	}
	def dumpSmallSampleOfTris(pairly: Stream[Pure, (Int, Int)]) : Unit = {
		val dbgHead = "mkPureStreamOfPairs"
		val pairlyList = pairly.take(10).toList
		dbgNow(dbgHead, s"Pairly-sample-list .toList len = ${pairlyList.length}, contents = ${pairlyList}")
		val naiveTriStrm = pairly.map(pair => {
			myNaiveTriMaker.nowMkTriWithRandSidesForFixedPerim(pair._1, pair._2)
		})

		val outTxt: Try[String] = Try(myTryConsumer.dumpFinitePureStreamOfTrisIntoTxtBlock(naiveTriStrm))
		dbgNow(dbgHead, s"NAIVE tris:\n=================\n${outTxt}\n===================")
	}

	def dumpSomeTrisToList(tripleStrm : Stream[IO, (Int, Int, Int)]) : IO[List[(Int, Int, Int)]] = {
		val dbgHead = "dumpSomeTrisToList"
		val compStrm = tripleStrm.debug().compile
		val sidesTupleListEff: IO[List[(Int, Int, Int)]] = compStrm.toList
		// FlatTap inserts an effect without changing the output
		val tappedEff: IO[List[(Int, Int, Int)]] = sidesTupleListEff.flatTap(sidesTupleList => {
			IO.println(s"${dbgHead}.tappedEff got SidesTuple-list: ${sidesTupleList}")
		})
		tappedEff // Will execute our tapped-print, and then produce the List

	}
	def sampleTriResultsWithCnts[SomeTriErrMsg](eithPlusCntsStrm : Stream[IO, (Either[SomeTriErrMsg, TriShapeXactish], (Int, Int))]) : IO[Unit] = {
		val front = eithPlusCntsStrm.take(5)
		val back = eithPlusCntsStrm.takeRight(5)
		val sample = front.append(back)
		val flg_dumpAll = false
		val dumpMe = if (flg_dumpAll) eithPlusCntsStrm else sample
		val lstEff = dumpMe.compile.toList
		val newline = "\n"
		val outJob: IO[Unit] = lstEff.flatMap (lst => IO.println(s"eithers with counts: ${lst.mkString(newline)}"))
		outJob
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

/*
DELETE:

val someInts = myOtherNums.streamNumsUsingUnfold
val outInts = someInts.toList
dbgNow(dbgHead, s"Dumped someInts=${someInts} as outInts=${outInts}")
val perimsRange = Stream.range(10, 4000, 3)
dbgNow(dbgHead, s"Rangey stream sample .toList = ${perimsRange.take(10).toList}")
val pairly: Stream[Pure, (Int, Int)] = perimsRange.flatMap(perim =>
		Stream.range(1, perim / 5).map(minLen => (perim, minLen)))
val pairlyList = pairly.take(10).toList
dbgNow(dbgHead, s"Pairly-sample-list .toList len = ${pairlyList.length}, contents = ${pairlyList}")
val naiveTriStrm = pairly.map(pair => {
	myNaiveTriMaker.nowMkTriWithRandSidesForFixedPerim(pair._1, pair._2)
})

val outTxt: Try[String] = Try(myTryConsumer.dumpFinitePureStreamOfTrisIntoTxtBlock(naiveTriStrm))
dbgNow(dbgHead, s"NAIVE tris:\n=================\n${outTxt}\n===================")
pairly
 */

//		val sideStrmDebugCompiledJob = sidesStreamJob.map(_.debug().compile) // type= IO[Stream.CompileOps[IO, IO, (Int, Int, Int)]]
//		val sideStrmLstDumpJob: IO[List[(Int, Int, Int)]] = sideStrmDebugCompiledJob.flatMap(compStrm => {


/*			val compStrm = tstrm.debug().compile
			val sidesTupleListEff: IO[List[(Int, Int, Int)]] = compStrm.toList
			// FlatTap inserts an effect without changing the output
			val tappedEff: IO[List[(Int, Int, Int)]] = sidesTupleListEff.flatTap(sidesTupleList => {
				IO.println(s"${dbgHead}.tappedEff got SidesTuple-list: ${sidesTupleList}")
			})
			tappedEff // Will execute our tapped-print, and then produce the List
 */
/*
val front = eithPlusCntsStrm.take(5)
val back = eithPlusCntsStrm.takeRight(5)
val sample = front.append(back)
val flg_dumpAll = false
val dumpMe = if (flg_dumpAll) eithPlusCntsStrm else sample
val lstEff = dumpMe.compile.toList
val newline = "\n"
lstEff.flatMap (lst => IO.println(s"eithers with counts: ${lst.mkString(newline)}"))
 */
