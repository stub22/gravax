package fun.gravax.xtyp.catty
import cats.{Monoid, Semigroup}

private trait MonoidStuff

trait NuttyMonoids {
	def someSemigroupAntics : String = {
		val x: String = Semigroup[String].combine("Hi ", "there")
		x
	}
}