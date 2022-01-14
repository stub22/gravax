package test.gravax.trimem

import org.scalatest.flatspec.AnyFlatSpec

private trait FakeUnisonEmuSpecs

class FirstFakeUnisonEmuSpec() extends AnyFlatSpec {
	val myUEO = new UnisonEmuOps {}
	"A UnisonEmu" should "do good stuff" in {
		myUEO.go
	}

}
trait UnisonEmuOps {
	def go : Unit = {
		println("UEO says blech via println")
	}
}