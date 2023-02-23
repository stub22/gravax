package fun.gravax.xtyp.fstrm

import cats.effect.IO
import fs2.{Pipe, Pure, Stream}
import fun.gravax.xtyp.mathy.tridesc.{TriShape, TriShapeXactish}

private trait TriStreamConsumerStuff

class TriSetStat(numTris : Int, totalArea : Double, totalPerim : Double)  {
	def getNumTris = numTris
	def getTotalArea = totalArea
	def getTotalPerim = totalPerim
	def getAvgArea = getTotalArea / getNumTris
	def getAvgPerim = getTotalPerim / getNumTris

	def combine(otherTSS : TriSetStat) = new TriSetStat(otherTSS.getNumTris + numTris, otherTSS.getTotalArea + totalArea, otherTSS.getTotalPerim + totalPerim)
	def addTri(nxtTri : TriShape) = {
		val tmpStat = nxtTri match {
			case tsx : TriShapeXactish => {
				val areaDouble = tsx.area.toDouble
				// https://docs.oracle.com/javase/7/docs/api/java/math/MathContext.html
				// val areaDecim = tsx.area.toBigDecimal()
				val perimDouble = tsx.perimeter.toDouble
				new TriSetStat(1, areaDouble, perimDouble)
			}
		}
		combine(tmpStat)
	}
	override def toString : String = s"[num=${numTris}, totAr=${totalArea}, avgAr=${getAvgArea}, totPerim=${totalPerim}, avgPerim=${getAvgPerim}"
}

trait TriStreamConsumer {
	private def myZeroStat = new TriSetStat(0, 0.0, 0.0)
	def summarizeTriStream[Eff[_]](strmOfTri : Stream[Eff, TriShape]) : Stream[Eff, TriSetStat] = {
		strmOfTri.fold(myZeroStat)((prevRslt, nxtTri) => {
			prevRslt.addTri(nxtTri)
		})
	}
	def dumpFinitePureStreamOfTrisIntoTxtBlock(pureStrmOfTri : Stream[Pure, TriShape]) : String = {
		val trisWithIdx: Stream[Pure, (TriShape, Long)] = pureStrmOfTri.zipWithIndex
		val outLinesStrm: Stream[Pure, String] = trisWithIdx.map(tup => s"tri #=${tup._2} as txt=${triShapeToTxt(tup._1)}}")
		// FIXME: Combining Strings with + is wasteful
		// emits a single element stream"
		val outBlockStrm: Stream[Pure, String] = outLinesStrm.reduce((s1, s2) => s1 + "\n" + s2)
		// Behold an explicit use of implicit conversion to a type defined in the companion object for stream.
		// implicit final class PureOps[O](self : fs2.Stream[fs2.Pure, O]) extends scala.AnyVal {
		val heyPureOps : Stream.PureOps[String] = outBlockStrm
		// Here the same implicit conversion is hiding.  .toList comes from PureOps
		val outTxt = outBlockStrm.toList.toString()
		outTxt
	}
	// Also notice  Stream.debug
	def dumpFiniteEffectfulStreamOfTrisIntoTxtStream[F[_]](pureStrmOfTri : Stream[F, TriShape]) : Stream[F, String] = {
		val trisWithIdx: Stream[F, (TriShape, Long)] = pureStrmOfTri.zipWithIndex
		val dumps: Stream[F, String] = trisWithIdx.map(tup => s"tri #=${tup._2} as txt=${triShapeToTxt(tup._1)}}")
		val dumpsWithNewlines : Stream[F, String] = dumps.intersperse("\n")
		// IMHO "fold" should return a subtype that indicates known-singleton-ness.
		// If we pass a mutable value into fold we are probably gonna have trouble!
		val outputsManyStrings =  dumpsWithNewlines
		// val outBlockStrm: Stream[F, String] = outLinesStrm.reduce((s1, s2) => s1 + "\n" + s2)

		// We can't compile the stream here, because our effect type is abstract.
		// .compile requires:  (implicit compiler : fs2.Compiler[F2, G])
		// val outTxt = outBlockStrm.compile.toVector
		outputsManyStrings
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

	The eval function works for any effect type, not just IO.
	FS2 does not care what effect type you use for your streams.
	You may use IO for effects or bring your own, just by implementing a few interfaces for your effect type
	(e.g., cats.MonadError[?, Throwable], cats.effect.Sync, cats.effect.Async, cats.effect.Concurrent).
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

	def triShapeToTxt(ts : TriShape) : String = {
		val sides = ts.sidesIncreasing
		val perim = ts.perimeter
		val area = ts.area
		s"[sides=${sides}, perim=${perim}, area=${area}]"
	}
}

