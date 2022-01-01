package test.gravax

import fun.gravax.gravnum.{NumFactoryImpl, ZeroPN}
import org.scalatest.flatspec.AnyFlatSpec

private trait FakeAxLamTstSpcs

class FirstAxLamSpec extends AnyFlatSpec {
	"A data factory" should "make some data" in {
		val goodFactory = ???
	}
}

class FirstTriSpec extends AnyFlatSpec {
	val myNumFact = new NumFactoryImpl()

}
