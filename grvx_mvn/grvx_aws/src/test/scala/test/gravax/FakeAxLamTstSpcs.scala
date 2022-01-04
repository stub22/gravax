package test.gravax

import fun.gravax.axlp.core.num.NumFactoryImpl
import org.scalatest.flatspec.AnyFlatSpec
import test.gravax.trial.Tag_NoBatch

private trait FakeAxLamTstSpcs

class FirstAxLamSpec extends AnyFlatSpec {
	val myNumFact = new NumFactoryImpl()
	"A number factory" should "make some numbers" taggedAs(Tag_NoBatch) in {
		val pos79 = myNumFact.mkSmallPosIntPN(79)

	}
}

class FirstTriSpec extends AnyFlatSpec {
	val myNumFact = new NumFactoryImpl()
	val myTF = new TSL_Factory()


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
