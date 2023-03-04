package fun.gravax.xtyp.effect

private trait ExploreEffectsV3

import cats.Eval
import cats.effect.{IO, IOApp}

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Try}

/*
https://typelevel.org/cats-effect/api/2.x/cats/effect/IO.html	(IO class API - version 2.x)
https://typelevel.org/cats-effect/api/2.x/cats/effect/IO$.html  (companion object)
Focus on error handling methods
https://softwaremill.com/practical-guide-to-error-handling-in-scala-cats-and-cats-effect
 */

object RunSomeCatsEffectExperiments extends IOApp.Simple {
	// IO.println has access to a Show
	val saysHello: IO[Unit] = IO.println("This IO.println job is now saying:  Hello, World!")

	override def run: IO[Unit] = {
		doSomeTryStuff

		val helloJob = saysHello
		val moneyReportJob: IO[String] = makeJobThatMakesJobThatDoesMoneyStuffAndReports

		val  jobChain: IO[String] = helloJob.flatMap(u => moneyReportJob)

		// productR and productL are equivalent to the *> and <* operators.
/*
	  def product[B](that: IO[B]): IO[(A, B)] =
		flatMap(a => that.map(b => (a, b)))
	  def productL[B](that: IO[B]): IO[A] =
		flatMap(a => that.as(a))
	  def productR[B](that: IO[B]): IO[B] =
		flatMap(_ => that)
 */

		val otherJobChain: IO[String] = helloJob.productR(moneyReportJob)
		val chainReturningUnit: IO[Unit] = helloJob.productL(moneyReportJob)

		val chainWithPrintStep: IO[Unit] = otherJobChain.map(report => println(s"ReportResult: ${report}"))

		// .void is same as       .map(_ => ())
		val chainWithIgnoredResult: IO[Unit] = otherJobChain.void
		val justUnit: Unit = ()
		val sameThing: IO[Unit] = otherJobChain.as(justUnit)


		chainWithPrintStep

	}



	def makeJobThatMakesJobThatDoesMoneyStuffAndReports : IO[String] = {
		/*
https://typelevel.org/cats-effect/api/2.x/cats/effect/IO$.html
		defer[A](thunk: => IO[A]): IO[A]
Suspends a synchronous side effect which produces an IO in IO.
This is useful for trampolining (i.e. when the side effect is conceptually the allocation of a stack frame).
Any exceptions thrown by the side effect will be caught and sequenced into the IO.

		 */
		// Defer all the work of BUILDING the money job
		// Another way to do this is to build job = IO.apply (== IO.delay), then job.flatten
		val jobThatBuildsAndRunsMoneyReportJob : IO[String] = IO.defer {
			// Here we are building an IO to do the money job.
			val mm = new MoneyMaker {}
			val mj: IO[Money] = mm.mkJobWhichMayFail
			val mjAtt: IO[Either[Throwable, Money]] = mj.attempt
			val reportJob: IO[String] = mjAtt.map(r => {
				r match {
					case Left(t) => s"Oops we failed, t=${t}"
					case Right(m) => s"Yay we won, m=${m}"
				}
			})
			reportJob
		}
		jobThatBuildsAndRunsMoneyReportJob
	}



	def doSomeTryStuff : Unit = {
		val mm = new MoneyMaker {}
		mm.doTryStuff

		mm.doFutStuff

		val ej = mm.mkEval

		val ejTrial: Try[Money] = Try(ej.value)
		val loopyOut: Seq[Either[Throwable, Money]] =  {
			val range = 1 to 10
			// range.flatMap(x => ejTrial
			val out = range.map(x => {
				val trial = Try(ej.value)
				println(s"On loop ${x} we got moneyTry=${trial}")
				val hmm = trial.getOrElse(GoodMoney(-1.0f))
				val trialEith: Either[Throwable, Money] = trial.toEither
				trialEith
			})
			out
		}
		println(s"LoopyOut: ${loopyOut}")
	}
}

trait Money
case class GoodMoney(amount : Float) extends Money
case class FailureInfo(summary : String, exc : Exception)
trait MoneyMaker {
	val myRNG = new Random()
	def makeMoneyOrThrow(moneyProb : Float, minMoney : Float, maxMoney : Float) : Money = {
		val chance = myRNG.nextFloat()
		if (chance <= moneyProb) {
			val rangeSize = maxMoney - minMoney
			val indicator = myRNG.nextFloat()
			val amt = minMoney + indicator * rangeSize
			val rslt = GoodMoney(amt)
			rslt
		} else throw new Exception(s"Failed to create money, because chance=${chance} > thresh=${moneyProb}")
	}
	def mkJobWhichMayFail : IO[Money] = {
		val moneyJob: IO[Money] = IO.apply(makeMoneyOrThrow(0.77f, 150.0f, 550.0f))
		moneyJob
	}
	def mkEval : Eval[Money] = {
/*
https://typelevel.org/cats/datatypes/eval.html
Eval is a data type for controlling synchronous evaluation. Its implementation is designed to provide stack-safety at all times using a technique called trampolining.

https://www.javadoc.io/doc/org.typelevel/cats-docs_2.13/latest/cats/Eval.html

This type wraps a value (or a computation that produces a value) and can produce it on command via the .value method.

There are three basic evaluation strategies:

Now: evaluated immediately
Later: evaluated once when value is needed
Always: evaluated every time value is needed

The Later and Always are both lazy strategies while Now is eager. Later and Always are distinguished from each other
only by memoization: once evaluated Later will save the value to be returned immediately if it is needed again. Always will run its computation every time.

Eval supports stack-safe lazy computation via the .map and .flatMap methods, which use an internal trampoline to avoid stack overflows. Computation done within .map and .flatMap is always done lazily, even when applied to a Now instance.

It is not generally good style to pattern-match on Eval instances. Rather, use .map and .flatMap to chain computation, and use .value to get the result when needed. It is also not good style to create Eval instances whose computation involves calling .value on another Eval instance -- this can defeat the trampolining and lead to stack overflows.
 */
		val ejob: Eval[Money] = Eval.always(makeMoneyOrThrow(0.66f, 150.0f, 550.0f))
		ejob
	}
	def doTryStuff = {
		println("Making plzTryMe")
		// This
		val plzTryMe: Try[Money] = Try(makeMoneyOrThrow(0.5f, 150.0f, 550.0f))
		println(s"Got plzTryMe=${plzTryMe}, clz=${plzTryMe.getClass}")
	}
	def doFutStuff : Unit = {
		implicit val ec = ExecutionContext.global
		println("Making fut")
		// Creating the future starts it running, so this is not referentially transparent.
		// Future eagerly evaluates AND memoizes
		val fut = Future(makeMoneyOrThrow(0.44f, 150.0f, 550.0f))
		if (true) {
			Thread.sleep(100)
		}
		println(s"fut = ${fut}")
		/*
https://docs.scala-lang.org/overviews/core/futures.html

https://stackoverflow.com/questions/31194138/difference-between-returning-future-failedexception-and-throwing-an-exception
Calling Future { throw ex } and Future.failed(ex) will create an equivalent result.
However, using Future.failed is more efficient

Good explanation of Future chaining:
https://stackoverflow.com/questions/31641190/futures-map-vs-flatmap

https://stackoverflow.com/questions/44196088/why-future-has-side-effects
		 */
	}
	def mkSleepJob(sec : Long) : IO[Unit] = {
		import scala.concurrent.duration._
		val dur = FiniteDuration(sec, TimeUnit.SECONDS)
		val sleepJob: IO[Unit] = IO.sleep(dur)
		sleepJob
	}
}