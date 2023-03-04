package test.gravax.xtyp.histo

private trait LazyFoldyAnimals

sealed trait Animal {
	def getWeightInKG : Float
	def mate(other : Animal) : Animal = ???
}

case class Monkey() extends Animal {
	def eatBanana : Unit = ???

	override def getWeightInKG: Float = 25.5f
}
case class Rabbit() extends Animal {
	def eatGrass : Unit = ???
	override def getWeightInKG: Float = 4.2f
}
case object Eeyore extends Animal {
	override def getWeightInKG: Float = 350.7f
}

// absorbOne returns another absorber
trait Absorber[-X] {
	def absorbOne(x : X) : Absorber[X] = ???
}

trait EatEmUp {
	val m1 = Monkey()
	val r1 = Rabbit()
	val oopsRType: Rabbit.type = Rabbit
	val e1 = Eeyore
	val animList: List[Animal] = List(m1, r1)
	val rabbList: List[Rabbit] = List(r1)
	val monkList : List[Monkey] = List(m1)
	val ambigList: List[Animal] = rabbList ++ monkList

	val monkeyAbsorber = new Absorber[Monkey]{} // Can absorb monkeys
	// May absorb any animal
	val genAbsorber = new Absorber[Animal] {}
	val rabbitAbsorber = new Absorber[Rabbit] {}

	def absorbAnimals : Unit = {
		monkeyAbsorber.absorbOne(m1)
		val nextRabAbs = rabbitAbsorber.absorbOne(r1)
		genAbsorber.absorbOne(r1).absorbOne(e1)
		// Since genAbsorber can absorb any animal, it can absorb monkeys
		val narrowAbsorber : Absorber[Monkey] = genAbsorber
		// The information about generality is lost, so can't do this
		// val reWidenAbsorber : Absorber[Animal] = narrowAbsorber // .asInstanceOf[Absorber[Animal]]
		narrowAbsorber.absorbOne(m1)
		// narrowAbsorber.absorbOne(r1)

		val rabbitList = List(r1)

		val el = List(Eeyore)

	}
	def doMatches ()
	def doFlatMaps
	def doForComps
	def doFoldSum : Unit = {
		val totalWeightRight : Float = animList.foldRight(0.0f)(_.getWeightInKG + _)

		val totalWeightLeft : Float = animList.foldLeft(0.0f)(_ + _.getWeightInKG)
		// val totalWeight : Float = animList.fold(0.0f)(_ + _)
		// trait Function1[-T, +R]
		val r = animList.reduce(_.mate(_))

	}
	def doTailRec
	def doCurry
	def doApply
	def doStream =  {
		// val x = Stream[Int].empty
	}
	def doView
	def doLazyColl
	def doScala3Union
	def doScala3Singleton	// Number 4 has a type

}
