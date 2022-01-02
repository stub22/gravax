package test.gravax

import fun.gravax.gravnum.{NumFactoryImpl, ZeroPN}
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
