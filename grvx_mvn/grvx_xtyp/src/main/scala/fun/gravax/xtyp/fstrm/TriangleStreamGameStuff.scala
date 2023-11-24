package fun.gravax.xtyp.fstrm

import cats.effect.kernel.Sync
import cats.FlatMap
import cats.effect.{ExitCode, IO, IOApp}
import fs2.timeseries.TimeStamped
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape, TriShapeXactish}
import fs2.{Pipe, Pure, Stream}
import fun.gravax.xtyp.histo.NumRangeHisto

import scala.concurrent.duration.FiniteDuration
import scala.util.Try


private trait TriangleStreamGameStuff

object RunTriStreamGame extends IOApp {

	val ourMGF = new MakesGameFeatures {}

	override def run(args: List[String]): IO[ExitCode] = {
		val helloJob: IO[Unit] = IO.println("RunTriStreamGame/run/helloJob asks:  Who wants to play with Triangles?")
		ourMGF.useCBoundClz
		val triJob = ourMGF.mkJobThatPrintsFewTris
		val manyJob = ourMGF.mkJobThatPrintsManyTris(200)
		val parJob = ourMGF.mkJobForParallelTris
		val oneHistJob = ourMGF.mkJobForWinnerHisto
		val multiHistJob = ourMGF.mkParJobForWinnerHistos(18)
		val totalHistJob = ourMGF.mkParJobForTotalHistos(400,
			288)
		val histJobs = oneHistJob.productR(multiHistJob).productR(totalHistJob)
		helloJob.productR(triJob).productR(manyJob).productR(parJob).productR(histJobs).as(ExitCode.Success)
	}
	// (IO(println("started")) >> IO.never).onCancel(IO(println("canceled")))

}
trait MakesGameFeatures {
	val ourTsMkr = new TriStreamMaker[IO] {
		override def getFM: FlatMap[IO] = IO.asyncForIO  // Uh, this "works"...
		override def getSync: Sync[IO] = Sync[IO]
	}
	type OurTriGenRslt = ourTsMkr.TriGenRslt	// Either
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

	// CONSIDER:  insert an fs2.Topic to allow different consumers to process the Either results.
	def mkJobProducingStreamOfTriEithers(maxPerim : Int): IO[Stream[IO, OurTriGenRslt]] = {
		val dbgHead = "mkJobProducingStreamOfTriEithers"

		// We start with a stream of int-pairs that each constrain a triangle-generation step.
		val pairStrmJob: IO[Stream[Pure, (Int, Int)]] = IO.apply {
			val pairStrm = myPieceMaker.mkPureStreamOfPairs(maxPerim)
			myPieceMaker.dumpSmallSampleOfTris(pairStrm)
			pairStrm
		}

		// FIXME:  From a readability perspective, the wrapping and unwrapping overwhelms the business code.
		// If we can put the business data transforms into methods of a trait, then invoke them all in
		// one long functional chain, that may be more readable.
		// A scala-idiomatic approach is to use a for-comprehension.
		// A categorical approach is to use Kleislis to component-ize our xforms.
		val flg_useParSidegen = false
		val sidesStreamJob: IO[Stream[IO, (Int, Int, Int)]] = if (flg_useParSidegen)
			makeParSidesTupleStreamJob(pairStrmJob)
		else
			makeSidesTupleStreamJob(pairStrmJob)

		// Hmm we are making these debug jobs now, but we won't use them until a bunch of other useful work
		// is done (which we might prefer to split into a different method).  Also we note that this job
		// will NOT see the same input as the more important triEithStrmJob below.  We can only get it
		// to see the same input if we place this work into the same chain, or use some kind of buffering
		// to share the data between jobs (e.g. fs2 Topic).
		val sideStrmLstDumpJob: IO[List[(Int, Int, Int)]] = sidesStreamJob.flatMap(tstrm => {
			myPieceMaker.dumpSomeTrisToList(tstrm)
		})

		// It is cumbersome to initialize all these 'job' vals.
		// One payoff is that we can later choose which of them actually get run, without having to disable the setup code.
		// However, we can't easily share these vals across methods.
		// If we replace each job-val with a job-def...
		val sidesEff: IO[Unit] = sidesStreamJob.flatMap(strm => {
			val dbgStrm = strm.debug()
			val listJob: IO[List[(Int, Int, Int)]] = dbgStrm.compile.toList
			val listDumpJob = listJob.flatMap(lst => IO.println(s"Side-Stream-list: ${lst}"))
			listDumpJob
		})
		val flg_useParTribuild = false
		val triEithStrmJob: IO[Stream[IO, OurTriGenRslt]] = sidesStreamJob.map(stupStrm => {
			if (flg_useParTribuild)
				stupStrm.parEvalMap(4)(sidesTup => ourTsMkr.mkXactTriJob(sidesTup))
			else
				stupStrm.evalMap(sidesTup => ourTsMkr.mkXactTriJob(sidesTup))

		})
		triEithStrmJob

	}

	def mkJobThatPrintsManyTris(maxPerim : Int): IO[Unit] = {
		val triEithStrmJob = mkJobProducingStreamOfTriEithers(maxPerim)
		val dbgHead = "mkJobThatPrintsManyTris"
		// Counting instances of Left+Right using zipWithScan.
		// Here we are embedding accumulator data into the record stream.
		val triEithWithCntsJob: IO[Stream[IO, (OurTriGenRslt, (Int, Int))]] =
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
		val sidesEff : IO[Unit] = IO.println("sidesEff is disabled") // sidesEff was built in old long-form of this method.
		val toDoChain = if(false) sidesEff >> summThenCnts else summThenCnts
		toDoChain
		// is >> exactly the same as productR?
		// >> [B](that: => IO[B]): IO[B]
		//Runs the current IO, then runs the parameter, keeping its result. The result of the first action is ignored.
		// If the source fails, the other action won't run. Evaluation of the parameter is done lazily, making this
		// suitable for recursion.
	}
	val myPipeOps = new TriStrmPipeOps{}
	val myUtilStrms = new SomeUtilityStreams {}
	def mkJobForParallelTris : IO[Unit] = {
		val triEithStrmJob = mkJobProducingStreamOfTriEithers(500)
		val dbgHead = "jobForParallelTris"
		val x: IO[Unit] = triEithStrmJob.flatMap((strm: Stream[IO, OurTriGenRslt]) => {
			/* broadcastThrough:
			**Error** Any error raised from the input stream, or from any pipe, will stop the pulling from this stream and from any pipe, and the error will be raised by the resulting stream.
			**Output**: the result stream collects and emits the outputs emitted from each pipe, mixed in an unknown way, with these guarantees:
			* 1. each output chunk was emitted by one pipe exactly once.
			* 2. chunks from each pipe come out of the resulting stream in the same order as they came out of the pipe, and without skipping any chunk.
			 */
			val countedTwice: Stream[IO, Int] = strm.broadcastThrough(myPipeOps.countTriFailures, myPipeOps.countTriFailures)
			val doubleCountJob = countedTwice.compile.toList.flatMap(lst => IO.println(s"${dbgHead} got parallel err-counts: ${lst}"))
			val timedDoubleCount = doubleCountJob.timed.map(pair => printIoDur("DOUBLE-COUNT")(pair))
			val goodPipe = myPipeOps.onlyWins _ andThen myPipeOps.accumStats // Stream[IO, Either[myPipeOps.OurTriErr, TriShapeXactish]] => Stream[IO, TriSetStat]
			val wideOut: Stream[IO, Any] = strm.broadcastThrough(myPipeOps.countTriFailures, goodPipe)
			val wideJob: IO[Unit] = wideOut.compile.toList.flatMap(lst => IO.println(s"${dbgHead} got parallel wide-out: ${lst}"))
			val wideJobTimed: IO[(FiniteDuration, Unit)] = wideJob.timed

			val wideJobTimeDumped = wideJobTimed.map(pair => printIoDur("WIDE")(pair))
			val winTimePipe = myPipeOps.onlyWins _ andThen myPipeOps.stampyShapes
			val stmpd: Stream[IO, TimeStamped[TriShape]] = strm.through(winTimePipe)
			val stmpdWithIdx: Stream[IO, (TimeStamped[TriShape], Long)] = stmpd.zipWithIndex
			val subset = stmpdWithIdx.filter(_._2 % 100 == 0)
			val timed: Stream[IO, FiniteDuration] = myUtilStrms.timedStream("STAMPED+INDEXED+SUBBED")
			val timedSub: Stream[IO, (TimeStamped[TriShape], Long)] = timed.flatMap(dur => subset)
			val stmpJob = timedSub.compile.toList.flatMap(lst => IO.println(s"${dbgHead} got stamped+indexed sample out: ${lst}"))
			val outerWRD = stmpJob.timed.map(pair => printIoDur("STAMPY-OUTER")(pair))
			val comboJob = timedDoubleCount.both(wideJobTimeDumped).both(outerWRD).timed.map(pair => printIoDur("COMBO")(pair)) //  pair => {println(s"Combo job got duration via IO.timed: ${pair._1}")})
			comboJob
		})
		x
	}
	def mkJobForWinnerHisto : IO[Unit] = {
		val triEithStrmJob = mkJobProducingStreamOfTriEithers(300)
		val dbgHead = "jobForWinnerHisto"
		val hsm = new TriHistoStreamMaker {}

		val hpj: IO[Unit] = triEithStrmJob.flatMap((strm: Stream[IO, OurTriGenRslt]) => {
			val oneHistoRslt: Stream[IO, NumRangeHisto] = strm.through(hsm.foldToOneHistoResult) // , hsm.scanToPartialHistoResults)
			val timed =  myUtilStrms.timedStream("FOLD-TO-HISTO")
			val oneHistoGrabJob: IO[List[NumRangeHisto]] = timed.flatMap(dur => oneHistoRslt).compile.toList
			val oneHistoDumpJob = oneHistoGrabJob.flatMap(lst => IO.println(s"${dbgHead} got histo-list: ${lst}"))
			oneHistoDumpJob
		})
		hpj
	}
	def mkParJobForWinnerHistos(numStrms : Int) : IO[Unit] = {
		val triEithStrmJob = mkJobProducingStreamOfTriEithers(400)
		val dbgHead = "parJobForWinnerHistos"
		val hsm = new TriHistoStreamMaker {}

		val hpj: IO[Unit] = triEithStrmJob.flatMap((strm: Stream[IO, OurTriGenRslt]) => {
			// Here we use the strm (program/data-generator) a total of numStrms times.
			val multipleFoldingStreams: Stream[Pure, Stream[IO, NumRangeHisto]] = Stream.range(0, numStrms).map(n => {
				val oneHistoRslt: Stream[IO, NumRangeHisto] = strm.through(hsm.foldToOneHistoResult)
				val timed = myUtilStrms.timedStream(s"PAR-FOLD-TO-HISTO_${n}")
				timed.flatMap(dur => oneHistoRslt)
			})
			// Run each of the numStrms gen+filter+fold jobs in parallel, as compute resources become available.
			// Each of the numStrms histogram results will be a single result in the joined stream, in no particular order.
			val joined: Stream[IO, NumRangeHisto] = multipleFoldingStreams.parJoinUnbounded
			// We can either output all the histos, or condense them into one.
			// Either way, the formal return type is Stream[IO, NumRangeHisto]
			val flg_condenseHistos = true
			val streamToRun : Stream[IO, NumRangeHisto] = if (flg_condenseHistos) {
				val jd = joined.debugChunks()
				jd.reduce((histo1, histo2) => histo1.combineAssumingBinsMatch(histo2))
			} else joined

			// The output list will either have one condensed histo, or numStrms separate histos.
			val multiHistoGrabJob: IO[List[NumRangeHisto]] = streamToRun.compile.toList
			val multiHistoDumpJob = multiHistoGrabJob.flatMap(lst => {
				val totalSamples = lst.foldLeft(0)((prevSum, nxtHisto) => {
					prevSum + nxtHisto.totalSampleCount
				})
				IO.println(s"${dbgHead} got totalSampleCount=${totalSamples} for histo-list: " + lst.mkString("\n"))
			})
			multiHistoDumpJob
		})
		hpj
	}
	// TODO:  Make a version of this that is able to run parallel folds over pieces of a concrete input data stream
	//  (as if coming from a file).
	def mkParJobForTotalHistos(maxPerim : Int, numStrms : Int) : IO[Unit] = {
		val triEithStrmJob = mkJobProducingStreamOfTriEithers(maxPerim)
		val dbgHead = "parJobForTotalHistos"
		val hsm = new TriHistoStreamMaker {}

		val thpj: IO[Unit] = triEithStrmJob.flatMap((strm: Stream[IO, OurTriGenRslt]) => {
			// Here we use the strm (program/data-generator) a total of numStrms times.
			// Each of those times will produce *DIFFERENT* data.
			val multipleFoldingStreams: Stream[Pure, Stream[IO, HistoForFailureOrNumRange]] = Stream.range(0, numStrms).map(n => {
				val oneHistoRslt: Stream[IO, HistoForFailureOrNumRange] = strm.through(hsm.foldToOneTotalResult)
				val timed = myUtilStrms.timedStream(s"PAR-TOTAL-HISTO_${n}")
				timed.flatMap(dur => oneHistoRslt)
			})
			// .parJoinUnbounded is the magical step!
			val joined: Stream[IO, HistoForFailureOrNumRange] = multipleFoldingStreams.parJoinUnbounded
			val flg_condenseHistos = true
			val streamToRun: Stream[IO, HistoForFailureOrNumRange] = if (flg_condenseHistos) {
				val jd = joined.debugChunks()
				jd.reduce((histo1, histo2) => histo1.combine(histo2))
			} else joined
			// The output list will either have one condensed histo, or numStrms separate histos.
			val multiHistoGrabJob: IO[List[HistoForFailureOrNumRange]] = streamToRun.compile.toList
			val multiHistoDumpJob = multiHistoGrabJob.flatMap(lst => {
				val totalSamples = lst.foldLeft(0)((prevSum, nxtHisto) => {
					prevSum + nxtHisto.totalSampleCount
				})
				IO.println(s"${dbgHead} got totalSampleCount=${totalSamples} for histo-list: " + lst.mkString("\n"))
			})
			multiHistoDumpJob
		})
		thpj
	}
	def printIoDur(heading: String)(ioTimedOutPair : (FiniteDuration, _)) : Unit = {
		println(s"${heading} got duration via IO.timed: ${ioTimedOutPair._1}")
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
	def makeParSidesTupleStreamJob(pairStreamJob: IO[Stream[Pure, (Int, Int)]]): IO[Stream[IO, (Int, Int, Int)]] = {
		import cats.effect.std.{Random => CatsRandom}
		val rngMakerJob: IO[CatsRandom[IO]] = CatsRandom.scalaUtilRandom[IO]
		val bothPrecursors: IO[(Stream[Pure, (Int, Int)], CatsRandom[IO])] = pairStreamJob.both(rngMakerJob)
		val sidesTupleStreamJob: IO[Stream[IO, (Int, Int, Int)]] = bothPrecursors.map(precPair => {
			val (pairStrm, rng) = precPair

			// ourTsMkr.makeTriSidesStreamUsingEvalMap(rng, pairStrm.covary[IO])	// uses Stream.PureOps.covary
			// Using any of the parEvalMap variants seems to impose an overall performance penalty.
			// Presumably this indicates that the job is "too small" to benefit.
			pairStrm.covary[IO].parEvalMapUnbounded(paramPair => {
				ourTsMkr.makeTriSidesJob(rng, paramPair._1, paramPair._2)
			})
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

	// Generates pairs of (perimeter, min-side-length)
	def mkPureStreamOfPairs(maxPerim : Int) : Stream[Pure, (Int, Int)] = {
		val dbgHead = "mkPureStreamOfPairs"
		val someInts = myOtherNums.streamNumsUsingUnfold
		val outInts = someInts.toList
		dbgNow(dbgHead, s"Dumped someInts=${someInts} as outInts=${outInts}")
		val perimsRange = Stream.range(10, maxPerim, 3)
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

