package test.gravax

import fun.gravax.axlp.core.num.{PracticeFreeNumFactory, PosIntPN, PositivePN, ProofPositive, PureNumBaseImpl, PurePosIntFactory, SmallFreeIntFactory}
import org.scalatest.flatspec.AnyFlatSpec
import test.gravax.trial.Tag_NoBatch

private trait FakeAxLamTstSpcs

class FirstAxLamSpec extends AnyFlatSpec {
	val myNumFact = new PracticeFreeNumFactory {
		val mySFIF = new SmallFreeIntFactory() {
		}
		override val myFreeIntFactory: SmallFreeIntFactory = mySFIF
	}

	"A number factory" should "make some numbers" taggedAs(Tag_NoBatch) in {
		val pos79: PosIntPN = myNumFact.myFreeIntFactory.mkSmallPosIntPN(79)
		val pos33: PosIntPN = myNumFact.myFreeIntFactory.mkSmallPosIntPN(33)
		println(s"pos79=${pos79.toString}, pos33=${pos33.toString}")
		val bi79: Option[BigInt] = pos79.asScalaBigInt
		println(s"bi79=${bi79}")
		val sumPIPN: PosIntPN = pos33.plusPIPN(pos79)
		val sumBI = sumPIPN.asScalaBigInt
		println(s"sum=${sumPIPN}, sumBI=${sumBI}")

	}
}

trait TslHelper {
	val myPFF = new PracticeFactoryFactory
	val mySideNumFact: PurePosIntFactory[PracticeSideNum] = myPFF.mySideNumFact
	val mySideLenFact: PracticeFreeNumFactory = myPFF.mySideLenFact
	val myLengthyIntFact: SmallFreeIntFactory = mySideLenFact.myFreeIntFactory
	val myTslFactory: TSL_Factory[PracticeSideNum, PositivePN] =  myPFF.myTslFact

	def mkTriFromSmallPosInts(sideLenA : Int, sideLenB : Int, sideLenC :Int) : TslFeatures = {
		val posA: PosIntPN = myLengthyIntFact.mkSmallPosIntPN(sideLenA)
		val posB = myLengthyIntFact.mkSmallPosIntPN(sideLenB)
		val posC = myLengthyIntFact.mkSmallPosIntPN(sideLenC)
		val tsl: TslFeatures	 = myTslFactory.mkTSL(posA, posB, posC)
		tsl
	}
}

class FirstTriSpec extends AnyFlatSpec {
	// type OurSideNum = PracticeSideNum
	// type OurSideLen =
	val myHelper = new TslHelper {}

	"A TSL factory" should "make some triangle side lengths" taggedAs(Tag_NoBatch) in {
		println("Hey I'm tryin to make some TSLs here")
		// First tri has integer sides
		val goodTsl5001 = myHelper.mkTriFromSmallPosInts(33,18,27)
		println(s"Made goodTsl5001=${goodTsl5001}")
		val slTup = goodTsl5001.getSideLengthsTuple
		println(s"slTup=$slTup")
		val perim = goodTsl5001.computePerimeter()
		println(s"perimiter=$perim")
	}

	"A TSL classifier" should "classify some TSLs to tell us which predicates they satisfy  " in {
		val goodTsl5002 = myHelper.mkTriFromSmallPosInts(17,14,23)

	}
}
/*
axLam : Yafl

axLam is an YaflFunc instance  triangle: RDF, JSON, Scala

Limited spaces of YaflFuncs are constructed as axLam func descriptions
as subtypes of YaflFunc01 which are one-arg functions.
Multi arg functions YaflFunc02... are sugar-only in our axLam impl.

Actual impls of AxLam funcs are of two kinds:
1) Authored functions describing typed Lambda functions of Ydat
2) Coded runtime funcs implementing equivalent of YaflFunc01 (Java/Scala or LLVM or ...) in a way that is "good enough"
for the mission.  Goodness of the functions is the subject of proofs.
*/

/* Quoting from scala package of 2.13.7 as decompiled by IDEA on 21-12-31

trait Function1[@scala.specialized -T1, @scala.specialized +R] extends scala.AnyRef {
  def apply(v1 : T1) : R
  @scala.annotation.unspecialized
  def compose[A](g : scala.Function1[A, T1]) : scala.Function1[A, R] = { /* compiled code */ }
  @scala.annotation.unspecialized
  def andThen[A](g : scala.Function1[R, A]) : scala.Function1[T1, A] = { /* compiled code */ }
  override def toString() : _root_.scala.Predef.String = { /* compiled code */ }
}
object Function1 extends scala.AnyRef {
  implicit final class UnliftOps[A, B] private[Function1] (f : scala.Function1[A, scala.Option[B]]) extends scala.AnyVal {
    def unlift : scala.PartialFunction[A, B] = { /* compiled code */ }
  }
}
 */
