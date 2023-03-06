package fun.gravax.zpol

private trait PetTypeStuff


// Trying variations on the type patterns explored in 'Returning the "Current" Type in Scala'
// http://tpolecat.github.io/2015/04/29/f-bounds.html
// Later found this:
// https://stackoverflow.com/questions/59813323/advantages-of-f-bounded-polymorphism-over-typeclass-for-return-current-type-prob
trait Pet { selfP =>
	type OutPet <: Pet	// Alternative to F-Bound (or is it alternative *form* of F-Bound?), without param type.  (Also no typeclass!)
	def renameOP(nNm : String) : OutPet		// This can be useful.  Requires subtypes to bind OutPet appropriately.
	def renameSlf(nNm : String) : selfP.type	// To construct a matching value, we need to use an explicit cast
	def getName : String
}
// selfP.type is defined the the type of the given instance of selfP, which is NOT a usable constructor.
// So it's not very useful as a return type
abstract class Mammal(myName : String) extends Pet {  selfM =>
	protected def mkAnotherOP(nm : String) : OutPet
	protected def mkAnotherSlf(nm : String) : selfM.type // This is defined as...whatever is the type of selfM, which is NOT a usable constructor
	override def renameOP(nwNm: String): OutPet = mkAnotherOP(nwNm)
	override def renameSlf(nwNm: String): selfM.type = mkAnotherSlf(nwNm)
	override def getName: String = myName
	def what : Unit = {
		def x : selfM.type = ???
		val m : Mammal = x
		/* We know that x is SOME SUBTYPE OF MAMMAL
				val y : selfM.type = m
		type mismatch;   found   : m.type (with underlying type fun.gravax.zpol.Mammal)     required: Mammal.this.type
		 */
		// Explicit cast to the rescue!
		val z : selfM.type = m.asInstanceOf[selfM.type]
	}
}

// Adding final does not help.
// selfK.type != Kitty although yeah selfK.type <: Kitty
final case class Kitty(kittyName : String) extends Mammal(kittyName) { selfK  =>
	override type OutPet = Kitty
	type What = selfK.type

	override protected def mkAnotherOP(nm: String): OutPet = new Kitty(nm)

	// Is selfK the *only* possible value of Kitty.this.type, by definition?
	// selfK is the wrong result for this method, but is the only value that typechecks.
	// Ah, but we can use explicit cast.
	override protected def mkAnotherSlf(nm: String) = {
		val wrongKittyRightType = selfK
		val copyKittyHasWrongType = selfK.copy(nm) // Nope this is just type Kitty
		val goodKitty = new Kitty(nm)
		val betterKitty = goodKitty.asInstanceOf[selfK.type]	// Explicit cast works
		betterKitty
		// selfK // ??? // selfK.copy(kittyName = selfK.kittyName)
	}
	/*
type mismatch;
 found   : fun.gravax.zpol.Kitty    required: Kitty.this.type
 	override protected def mkAnotherSlf(nm: String) = new Kitty(nm)
	 */
	// with selfK.type
}
trait PetLover {
	def go : String = {
		val ger = new Kitty("gerald")
		val hcliff: Kitty = ger.renameOP("heathcliff")
		val hEsqSlf: Kitty = esqSlf(hcliff)
		val hMD: Kitty =  mdOP(hcliff)
		val hPHD: hcliff.type = huhPHD(hcliff)
		val hkChk : Kitty = hPHD
		val hMBA: Kitty = wowMBA(hcliff)
		val gerMam : Mammal = ger
		val hAnon : Pet = hcliff
		val borg: hAnon.OutPet = hAnon.renameOP("borg")
		val p : Pet = borg

		val kLst = List[Kitty](ger, hcliff, hEsqSlf, hMD, hPHD)
		val pLst = List[Pet](ger, hcliff, hAnon, borg, gerMam)
		val out : String = s"kLst={$kLst}\npLst={$pLst}"
		out
	}
	def esqSlf[PT <: Pet](p : PT) : PT = p.renameSlf(p.getName + ", esq.")	// Does not work
	def mdOP[PT <: Pet](p : PT) : PT#OutPet = p.renameOP(p.getName + ", M.D.")

	// Polymorphic return type, without using type parameter.
	def huhPHD(p : Pet) : p.type = p.renameSlf(p.getName + ", PhD")

	def wowMBA(p : Pet) : p.OutPet = p.renameOP(p.getName + ", MBA")
}
/*
implicit class PetOps[A](a: A)(implicit ev: Pet[A]) {
	def name = ev.name(a)
	def renamed(newName: String): A = ev.renamed(a, newName)
}
 */
