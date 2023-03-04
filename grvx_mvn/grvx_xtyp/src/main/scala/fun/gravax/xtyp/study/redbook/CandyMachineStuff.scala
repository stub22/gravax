package fun.gravax.xtyp.study.redbook

import cats.syntax.compose

private trait CandyMachineStuff

//  S is actual darn state.  So what is "StateXf"?  It is a wrapper for a state-mapping func called run.
// "run" takes an S input and tells us what the output S and A should be.
case class StateXf[S, +A](run: S => (A, S)) {
	def map[B](f: A => B): StateXf[S, B] =
		flatMap(a => StateXf.unit(f(a)))
	def map2[B,C](sb: StateXf[S, B])(f: (A, B) => C): StateXf[S, C] =
		flatMap(a => sb.map(b => f(a, b)))
	def flatMap[B](f: A => StateXf[S, B]): StateXf[S, B] = StateXf(s => {
		val (a, s1) = run(s)
		f(a).run(s1)
	})
}

object StateXf {
	// type Rand[A] = StateXf[RNG, A]

	def unit[S, A](a: A): StateXf[S, A] =
		StateXf(s => (a, s))

	// The idiomatic solution is expressed via foldRight
	def sequenceViaFoldRight[S,A](sas: List[StateXf[S, A]]): StateXf[S, List[A]] =
		sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

	// This implementation uses a loop internally and is the same recursion
	// pattern as a left fold. It is quite common with left folds to build
	// up a list in reverse order, then reverse it at the end.
	// (We could also use a collection.mutable.ListBuffer internally.)
	def sequence[S, A](sas: List[StateXf[S, A]]): StateXf[S, List[A]] = {
		def go(s: S, actions: List[StateXf[S,A]], acc: List[A]): (List[A],S) =
			actions match {
				case Nil => (acc.reverse,s)
				case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
			}
		StateXf((s: S) => go(s,sas,List()))
	}

	// We can also write the loop using a left fold. This is tail recursive like the
	// previous solution, but it reverses the list _before_ folding it instead of after.
	// You might think that this is slower than the `foldRight` solution since it
	// walks over the list twice, but it's actually faster! The `foldRight` solution
	// technically has to also walk the list twice, since it has to unravel the call
	// stack, not being tail recursive. And the call stack will be as tall as the list
	// is long.
	def sequenceViaFoldLeft[S,A](l: List[StateXf[S, A]]): StateXf[S, List[A]] =
		l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

	def modify[S](f: S => S): StateXf[S, Unit] = for {
		s <- get // Gets the current state and assigns it to `s`.
		_ <- set(f(s)) // Sets the new state to `f` applied to `s`.
	} yield ()

	def get[S]: StateXf[S, S] = StateXf(s => (s, s))

	def set[S](s: S): StateXf[S, Unit] = StateXf(_ => ((), s))
}



sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
	def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
		(i, s) match {
			case (_, Machine(_, 0, _)) => s
			case (Coin, Machine(false, _, _)) => s
			case (Turn, Machine(true, _, _)) => s
			case (Coin, Machine(true, candy, coin)) =>
				Machine(false, candy, coin + 1)
			case (Turn, Machine(false, candy, coin)) =>
				Machine(true, candy - 1, coin)
		}


	def simulateMachine(inputs: List[Input]): StateXf[Machine, (Int, Int)] = {
		// The wrapper is StateXf[Machine, _]
		// val out: StateXf[Machine, (Int, Int)] =
		for {
			// def sequence[S, A](sas: List[StateXf[S, A]]): StateXf[S, List[A]]
			// def modify[S](f: S => S): StateXf[S, Unit]
			// Behold an unreadable line of magic
			// AHA the underscore is performing an eta-expansion, so here the _ does not refer to the .map arg
			_ <- StateXf.sequence(inputs map (StateXf.modify[Machine] _ compose update))
			s <- StateXf.get
		} yield (s.coins, s.candies)
		// out
	}

	def junkJunk(inp : Input) : StateXf[Machine, Unit] = {
		// update is a curried method of 2 args:   Input => Machine => Machine
		val upFunc: Input => Machine => Machine = update
		// modify is a one-arg method, but the arg is a function.
		// Think this is an implied eta-expansion, turning the modify method into a function.
		val modFunc: (Machine => Machine) => StateXf[Machine, Unit] = StateXf.modify[Machine]
		// Adding the underscore makes it an explicit eta expansion
		val modFuncEta: (Machine => Machine) => StateXf[Machine, Unit] = StateXf.modify[Machine] _
		// https://stackoverflow.com/questions/39445018/what-is-the-eta-expansion-in-scala
		val composed: Input => StateXf[Machine, Unit] = modFuncEta compose update
		// Here is the expression used inside the map in the
		val composedAnother: Input => StateXf[Machine, Unit] = StateXf.modify[Machine] _ compose update
		// Applying input determines a Machine transformer
		val updateXformForThisInput: Machine => Machine = update(inp)
		val smm: (Machine => Machine) => StateXf[Machine, Unit] = StateXf.modify[Machine]
		val applied = smm(updateXformForThisInput)
		// val huh =

		// val x: StateXf[Machine, Unit] = StateXf.modify[Machine](machXform)
		???
	}
	def expandedSimulateMachine(inputs: List[Input]): StateXf[Machine, (Int, Int)] = {
		// case class StateXf[S, +A](run: S => (A, S))
		// "StateXf" is actually a mapper.  Here we partially apply the input to produce a mapper
		// that
		val composedMapper: Input => StateXf[Machine, Unit] = StateXf.modify[Machine] _ compose update
		// Produce a list of the state-xformers to be applied
		val stateOnlyMappers: List[StateXf[Machine, Unit]] = inputs map (composedMapper)
		// Now we apply all those xformers
		val seqd: StateXf[Machine, List[Unit]] = StateXf.sequence(stateOnlyMappers)
		// seqd.flatMap(x => {
		// 	val y: StateXf[Nothing, Nothing] = StateXf.get
		// })
		val out: StateXf[Machine, (Int, Int)] = for {
			_ <- seqd
			s <- StateXf.get
		} yield (s.coins, s.candies)
		out
	}
}
// See bottom of this page:
// https://www.scala-exercises.org/fp_in_scala/pure_functional_state

object RunCandyTests {
	def main(args: Array[String]): Unit = {
		goDoStuff
	}
	def goDoStuff : Unit = {
		val someInputs = List[Input](Turn, Coin, Coin, Turn, Turn, Turn, Coin, Coin, Coin, Coin)
		println(s"Inputs: ${someInputs}")
		val simulator: StateXf[Machine, (Int, Int)] = Candy.simulateMachine(someInputs)
		println(s"Absorbed inputs to produce sequence-applier:  ${simulator}")

		val mach0 = Machine(true, 100, 50)
		println("Start ")
		val outTuple: ((Int, Int), Machine) = simulator.run(mach0)
		println(s"Submitted start-state=${mach0} to simulator and produced output: ${outTuple}")

	}


}