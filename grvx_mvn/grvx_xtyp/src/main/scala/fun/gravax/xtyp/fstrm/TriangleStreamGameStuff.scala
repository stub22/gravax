package fun.gravax.xtyp.fstrm

import cats.effect.kernel.Sync
import cats.FlatMap
import cats.effect.{ExitCode, IO, IOApp}
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape}
import fs2.{Pipe, Pure, Stream}

import scala.util.Try


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
			val tshp = myNaiveTriMaker.nowMakeOneDummyTriShape
			val ttxt = myTryConsumer.triShapeToTxt(tshp)
			println(s"Dumped tshp=${tshp} as txt=${ttxt}")
			val randT = myNaiveTriMaker.nowMkTriWithRandSidesForFixedPerim(20, 4)
			println(s"Dumped randT=${tshp} as txt=${myTryConsumer.triShapeToTxt(randT)}")
			val strmOf2: Stream[Pure, TriShape] = Stream.emits(List(tshp, randT))
			val strmOf30 = strmOf2.repeatN(15)
			val strmOf9 = strmOf30.takeRight(9)
			val outTxt = myTryConsumer.dumpFinitePureStreamOfTrisIntoTxtBlock(strmOf9)
			println(s"9 tris:\n=================\n${outTxt}\n===================")
			val summStrm: Stream[Pure, TriSetStat] = myTryConsumer.summarizeTriStream(strmOf9)
			val summOut = summStrm.toList
			println(s"Summarized 9 tris as: ${summOut}")

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
		val omd: IO[Unit] = oneMore.flatMap(num => IO.println(s"Effect produced num: ${num}"))
		bigOlJobChain *> omd
	}

	def dbgNow(dbgHead: String, dbgBody: String): Unit = println(dbgHead, dbgBody)

	def mkJobThatPrintsManyTris: IO[Unit] = {
		val dbgHead = "mkJobThatPrintsManyTris"
		val pairlyEffect: IO[Stream[Pure, (Int, Int)]] = IO.apply {

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

		import cats.effect.std.{Random => CatsRandom}
		val rngMakerJob: IO[CatsRandom[IO]] = CatsRandom.scalaUtilRandom[IO]
		val bothPrecursors: IO[(Stream[Pure, (Int, Int)], CatsRandom[IO])] = pairlyEffect.both(rngMakerJob)
		val sidesEff: IO[Unit] = bothPrecursors.flatMap(precPair => {
			val (pairStrm, rng) = precPair
			val betterPairStrm: Stream[IO, (Int, Int)] = pairStrm.covary[IO]

			val triSidesStrm: Stream[IO, (Int, Int, Int)] = ourTsMkr.makeTriSidesStreamUsingEvalMap(rng, betterPairStrm)
			val dbgStrm = triSidesStrm.debug()
			val listJob: IO[List[(Int, Int, Int)]] = dbgStrm.compile.toList
			val listDumpJob = listJob.flatMap(lst => IO.println(s"Side-Stream-list: ${lst}"))
			listDumpJob
		})
		sidesEff
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
 */