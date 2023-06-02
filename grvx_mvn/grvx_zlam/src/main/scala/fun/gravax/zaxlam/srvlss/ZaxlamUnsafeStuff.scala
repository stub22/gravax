package fun.gravax.zaxlam.srvlss

import zio.{Exit, Task, Runtime => ZRuntime, Unsafe => ZUnsafe}

private trait ZaxlamUnsafeStuff

//
trait UnsafeTaskRunner {
	// Unit task produces no result, and may throw.
	def doRunUnitTaskNow(task : Task[Unit]) : Unit = {
		doRunUnsafeTaskMightThrow(task)
	}
	def doRunUnsafeTaskMightThrow[Rslt](task : Task[Rslt]) : Rslt = {
		// Extra-unsafe version for infra unit tests, which gives authentic stack trace for the fiber.
		// Cannot just call version because the getOrThrow methods want the same Unsafe-ctx as task ran in.
		println(s"======================= println: UnsafeTaskRunner.doRunUnsafeTaskMightThrow START, inputTask=${task}")
		val zioRuntime: ZRuntime[Any] = ZRuntime.default
		println(s"UnsafeTaskRunner zioRuntime=${zioRuntime}")
		val r : Rslt = ZUnsafe.unsafe { implicit unsafeThingy =>
			val u: zioRuntime.UnsafeAPI = zioRuntime.unsafe
			val rt: Exit[Throwable, Rslt] = u.run(task) // R
			val rsltOut: Rslt = rt.getOrThrowFiberFailure() // Uses unsafe implicit, probly for stack-trace
			rsltOut
//			zioRuntime.unsafe.run(task).getOrThrowFiberFailure()
		}
		println(s"======================== println: UnsafeTaskRunner.doRunUnsafeTaskMightThrow END with result class=${r.getClass.getName}")
		r
	}
	def doRunUnsafeTaskToExit[Rslt](task : Task[Rslt]) : Exit[Throwable, Rslt] = {
		println(s"======================= println: UnsafeTaskRunner.doRunUnsafeTaskToExit START, inputTask=${task}")
		val zioRuntime: ZRuntime[Any] = ZRuntime.default
		println(s"UnsafeTaskRunner zioRuntime=${zioRuntime}")
		val exOut : Exit[Throwable, Rslt] = ZUnsafe.unsafe { implicit unsafeThingy =>
			val apiInst: zioRuntime.UnsafeAPI = zioRuntime.unsafe
			val xit: Exit[Throwable, Rslt] = apiInst.run(task)
			xit
		}
		println(s"======================== println: UnsafeTaskRunner.doRunUnsafeTaskToExit END with exit class=${exOut.getClass.getName}")
		exOut
	}

	def doRunUnsafeTaskToEither[Rslt](task : Task[Rslt]) : Either[Throwable, Rslt] = {
		val xit = doRunUnsafeTaskToExit(task)
		xit.toEither
	}

}


/*
trait Runtime[+R] extends scala.AnyRef {
def environment : zio.ZEnvironment[R]

trait UnsafeAPI extends scala.AnyRef {
def run[E, A](zio : zio.ZIO[R, E, A])(implicit trace : zio.Trace, unsafe : zio.Unsafe) : zio.Exit[E, A]
def fork[E, A](zio : zio.ZIO[R, E, A])(implicit trace : zio.Trace, unsafe : zio.Unsafe) : zio.Fiber.Runtime[E, A]
def runToFuture[E <: scala.Throwable, A](zio : zio.ZIO[R, E, A])(implicit trace : zio.Trace, unsafe : zio.Unsafe) : zio.CancelableFuture[A]
}
def unsafe : Runtime.this.UnsafeAPI = { /* compiled code */ }

sealed trait Exit[+E, +A] extends scala.AnyRef with zio.ZIO[scala.Any, E, A] {
https://zio.dev/reference/core/exit/
An Exit[E, A] value describes how fibers end life. It has two possible values:

Exit.Success contain a success value of type A.
Exit.Failure contains a failure Cause of type E.

 */
