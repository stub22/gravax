package fun.gravax.xtyp.mathy

import spire.math.Complex

private trait WackySpireMathStuff

trait MakeSpireDoSomethinCool {

	def doSomeCmplxStf : Unit = {

		val a = 3.0
		val b = 5.0
		val c = Complex(a, b)
		println(s"Made c=${c}")
	}
}