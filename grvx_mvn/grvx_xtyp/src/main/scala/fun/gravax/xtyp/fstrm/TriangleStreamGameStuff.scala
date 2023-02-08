package fun.gravax.xtyp.fstrm

import cats.effect.{IO, IOApp}
import fun.gravax.xtyp.mathy.TriSideRec
import fun.gravax.xtyp.mathy.tridesc.{MakesTSX, TriShape}
import fs2.{Pure, Stream}

import scala.util.Random

private trait TriangleStreamGameStuff

object RunTriStreamGame extends IOApp.Simple {

	val ourMGF = new MakesGameFeatures {}
	def run:  IO[Unit]= {
		val helloJob: IO[Unit] = IO.println("RunTriStreamGame asks:  Who wants to play with Triangles?")
		val triJob = ourMGF.mkJobThatPrintsFewTris
		val manyJob = ourMGF.mkJobThatPrintsManyTris
		helloJob.productR(triJob).productR(manyJob)
	}


}
trait MakesGameFeatures {
	val ourTsMkr = new TriStreamMaker {}
	def mkJobThatPrintsFewTris : IO[Unit] = {
		IO.apply {
			val tshp = ourTsMkr.makeTriShape
			val ttxt = triShapeToTxt(tshp)
			println(s"Dumped tshp=${tshp} as txt=${ttxt}")
			val randT = ourTsMkr.mkTriWithRandSidesForFixedPerim(20, 4)
			println(s"Dumped randT=${tshp} as txt=${triShapeToTxt(randT)}")
			val strmOf2: Stream[Pure, TriShape] = Stream.emits(List(tshp, randT))
			val strmOf30 = strmOf2.repeatN(15)
			val strmOf9 = strmOf30.takeRight(9)
			val nineWithIdx: Stream[Pure, (TriShape, Long)] = strmOf9.zipWithIndex
			val outLinesStrm: Stream[Pure, String] = nineWithIdx.map(tup => s"tri #=${tup._2} as txt=${triShapeToTxt(tup._1)}}")
			val outBlockStrm: Stream[Pure, String] = outLinesStrm.reduce((s1, s2) => s1 + "\n" + s2)
			val outTxt = outBlockStrm.toList.toString()
			println(s"9 tris:\n=================\n${outTxt}\n===================")
		}
	}
	def mkJobThatPrintsManyTris : IO[Unit] = {
		IO.apply {
			val someInts = ourTsMkr.streamSomeInts
			val outInts = someInts.toList
			println(s"Dumped someInts=${someInts} as outInts=${outInts}")

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
	// FIXME:  The randomness makes it not really Pure
	def streamRandomTris(cnt_opt : Option[Int], perim : Int, minSideLen : Int) : Stream[Pure, TriShape] = {
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

}