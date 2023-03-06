package fun.gravax.zpol


import zio._
import zio.Console._

import java.io.IOException

private trait RunHelloZioStuff

object RunHelloZio extends ZIOAppDefault {

	def run = myAppLogic

	val myAppLogic: ZIO[Any, IOException, Unit] =
		for {
			_    <- printLine("Hello! What is your name?")
			name <- readLine
			_    <- printLine(s"Hello, ${name}, welcome to ZIO!")
			_	<- ZIO.log("This message written with ZIO.log")
			_	<- mkPetGameJob.refineToOrDie[IOException]
			_ 	<- moreLogStuff
			_	<- mkRosterGameJob.refineToOrDie[IOException]
		} yield ()

	def mkPetGameJob = ZIO.attempt {
		val pl = new PetLover {}
		val plOut = pl.go
		println(s"mkPetGameJob.println says plOut=${plOut}")
		// ZIO.log(s"plOut:\n${plOut}")
	}
	def mkRosterGameJob = ZIO.attempt {
		val rw = new RosterWorkout {}
		rw.go
	}
	def moreLogStuff = {
		for {
			_ <- ZIO.log("moreLogStuff offers MSG 001")
			_ <- ZIO.log("moreLogStuff offers MSG 002")
		} yield ()
	}
}

import zio._

trait ServiceA
trait ServiceB
trait ServiceC

trait Hmm {
	// Requires ServiceA and produces a value of type Int
	def foo: ZIO[ServiceA, Nothing, Int] = ???

	// Requires ServiceB and ServiceC and produces a value of type String
	def bar: ZIO[ServiceB & ServiceC, Throwable, String] = ???

	// Requires ServicB and produces a value of type Double
	def baz(a: Int, b: String): ZIO[ServiceB, Nothing, Double] = ???

	// Requires ServiceB and ServiceB and ServiceC and produces a value of type Double
	val myApp: ZIO[ServiceA & ServiceB & ServiceC, Throwable, Double] =
		for {
			a <- foo
			b <- bar
			c <- baz(a, b)
		} yield c
}

/*

private[zio] trait IntersectionTypeCompat extends scala.AnyRef {
  type &[+A, +B] = A with B
}


 */