package fun.gravax.xtyp.effect

private trait ExploreEffectsV3

import cats.effect.{IO, IOApp}

object RunSomeCatsEffectExperiments extends IOApp.Simple {
	val saysHello: IO[Unit] = IO.println("Here we are saying:  Hello, World!")

	override def run: IO[Unit] = saysHello
}


trait FizzBuzzExample {


}