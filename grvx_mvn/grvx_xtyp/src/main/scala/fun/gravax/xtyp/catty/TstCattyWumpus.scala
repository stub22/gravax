package fun.gravax.xtyp.catty

import fun.gravax.xtyp.mathy.MakeSpireDoSomethinCool

private trait TstCattyWumpus // SourceFileNameMarkerTrait

object RunCattyWumpus {
	def main(args: Array[String]): Unit = {
		goDoStuff
	}
	def goDoStuff : Unit = {
		println ("RunCattyWumpus doing stuff yay")
		val nmstf = new NuttyMonoids {}
		val xtxt = nmstf.someSemigroupAntics
		println(s"semigroup magic yields xtxt=${xtxt}")

		val scs = new MakeSpireDoSomethinCool{}
		scs.doSomeCmplxStf
	}
}
