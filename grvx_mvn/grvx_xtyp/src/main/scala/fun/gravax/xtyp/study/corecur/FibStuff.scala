package fun.gravax.xtyp.study.corecur

import cats.Eval

private trait FibStuff


object RunFibStuff {
	def main(args: Array[String]): Unit = {
		doBasicStuff
		doFibStuff
		doGenStuff
		doFactorialStuff
		doFoldRightStuff
		doMatchStuff
	}

	def doBasicStuff : Unit = {
		val lb = new LazyBasic {}
		println(s"Basic lazy list: ${lb.shortLL}")
	}
	def doFibStuff: Unit = {
		val lf = new LazyFibs {}
		val fibList01 = lf.fibFrom(0,1)
		val fibOut10 = fibList01(10)
		println(s"Fib #10 (starting from 0) is ${fibOut10}")
		println(s"The list is now: ${fibList01}")
		val fz = fibList01.zipWithIndex
		println(s"ZippedWithIndex = ${fz}")
		println(s"fz(10) = ${fz(10)}")
		println(s"fz is now: ${fz}")
	}
	def doGenStuff : Unit = {
		val dge = new DataGenExamples {}
		val gl = dge.generate(10)
		println(s"generated list of tuples: ${gl}, size=${gl.size} glAgain=${gl}")
		val first15: Seq[(Int, Int, Int)] = gl.take(15)
		println(s"First 15 = ${first15}, size = ${first15.size}")
		val l5 = first15.takeRight(5)
		println(s"Last 5 of first 15 =${l5}, size=${l5.size}, first15-AGAIN=${first15}, last5-AGAIN=${l5}")

		val wack = dge.otherGen
		println(s"otherGen = ${wack}")
		dge.genPerms


		val clist = dge.genConstrainedList(12, 79)

		val olist01 = dge.anotherCL(10, 50)
		val olist02 = dge.anotherCL(10, 15)

	}
	val ete = new EvalTrampolineExamples {}
	def doFactorialStuff : Unit = {

		val n = 50000
		val bigFact = ete.factorial(n)
		println(s"Factorial(${n}) = $bigFact")
		val bfv = bigFact.value
		val bfvTxt = bfv.toString()
		println(s"factorial.value has ${bfvTxt.length} digits, starts with ${bfvTxt.take(30)}...")

	}
	def doFoldRightStuff : Unit = {
		val data = (1 to 100000).toList
		val summed = ete.foldRight(data, 0L)(_ + _)
		// Regarding ".size" vs ".length" :
		// https://stackoverflow.com/questions/22966705/scala-what-is-the-difference-between-size-and-length-of-a-seq
		// https://stackoverflow.com/questions/20192843/difference-between-size-and-length-methods
		println(s"Summed input data of length=${data.length}")
	}
	
	def partialFuncStuff : Unit = { 
		val triplePF: PartialFunction[Int, Int] = {
			case x : Int => 3 * x
		}
		val someInts = List(1, 2, 3)
		val tripled: Seq[Int] = someInts.map(triplePF)
	}
	def doMatchStuff : Unit = {
		val mex = new MatchyExamples {}
		val list3 = List('a', 'b', 'c')
		val list4 = List('a', 'b', 'c', 'd')
		val list5 = List('a', 'b', 'c', 'd', 'e')
		mex.grabPieces(list3)
		mex.grabPieces(list4)
		mex.grabPieces(list5)
		val partlySilly = (1 to 10).toList
		val silly01 = mex.grabSillyItems(partlySilly)
		println(s"Got silly items: ${silly01}")
		val unsilly = List(0.5, 1.8, -12.2, 21.4)
		val noSillies = mex.grabSillyItems(unsilly)
		println(s"Nothing silly here: ${noSillies}")

		println (s"Labeled list3 as ${mex.decideLabel(list3)}")
		println (s"Labeled list4 as ${mex.decideLabel(list4)}")
		println (s"Labeled list5 as ${mex.decideLabel(list5)}")

	}

}

//   https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html

trait LazyBasic {
	val shortLL = 1 #:: 2 #:: 3 #:: LazyList.empty

}
trait LazyFibs {
	def fibFrom(a: Int, b: Int): LazyList[Int] = a #:: fibFrom(b, a + b)


}

trait DataGenExamples {
	// https://www.reddit.com/r/scala/comments/fdp4yz/scala_beginner_question_can_you_create_tuplesxyz/
	def genPerms: Unit = {
		val orig01 = "abc"
		val perms01 = orig01.permutations.toSeq
		println(s"Permutations of ${orig01} has length ${perms01.size}, contents: ${perms01}")
		val orig02 = "aba"
		val perms02 = orig02.permutations.toSeq
		println(s"Permutations of ${orig02} has length ${perms02.size}, contents: ${perms02}")
		val orig03 = Vector(7, 4, 5)
		val perms03 = orig03.permutations.toSeq
		println(s"Permutations of ${orig03} has length ${perms03.size}, contents: ${perms03}")
	}
	def generate(n: Int): LazyList[(Int, Int, Int)] =
		LazyList.unfold(Option((0, 0, 0))) {
			case None => None
			case Some(p @ (x, y, z)) =>
				if (z < y) Some(p -> Some((x, y, z + 1)))
				else if (y < x) Some(p -> Some((x, y + 1, 0)))
				else if (x < n) Some(p -> Some((x + 1, 0, 0)))
				else Some(p -> None)
		}

	def otherGen : Seq[(Int, Int, Int)] = {
		val n = 5
		val uniqueTriplets: Seq[(Int, Int, Int)] = Seq.unfold(Option(0, 0, 0)) {
			case None => None
			case Some(current) =>
				val maybeNext = current match {
					// Backticks are necessary to treat outer scope values as CONSTANTS in the patterns
					case (`n`, `n`, `n`) => None
					case (x, `n`, `n`) => Some(x + 1, x + 1, x + 1)
					case (x, y, `n`) => Some(x, y + 1, y + 1)
					case (x, y, z) => Some(x, y, z + 1)
				}
				Some(current, maybeNext)
		}
		uniqueTriplets
	}

	def genConstrainedList(listLen : Int, listSum : Int) : List[Int] = {
		val (initLen, initSum) = (0,0)
		//   def unfold[A, S](init : S)(f : scala.Function1[S, scala.Option[scala.Tuple2[A, S]]]) : CC[A] = { /* compiled code */ }
		val outList : List[Int] = List.unfold[Int, (Int, Int)]((initLen, initSum)) (tuple => {
			val (subLen, subSum) = tuple
			if (subLen == listLen) None
			else {
				val remainLen = listLen - subLen
				val remainSum = listSum - subSum
				val nextOut = remainSum / remainLen
				val nextLen = subLen + 1
				val nextSum = subSum + nextOut
				Some (nextOut, (nextLen, nextSum))
			}
		})
		println(s"Generated list of length=${listLen} and sum=${listSum}, contents=${outList}")
		val checksum = outList.reduce(_ + _)
		println(s"Checksum = ${checksum}")
		outList
	}

	def anotherCL (listLen : Int, listSum : Int) : List[Int] = {
		// https://www.oreilly.com/library/view/programming-scala-2nd/9781491950135/ch04.html
		// In case clauses, a term that begins with a lowercase letter is assumed to be the name of a new variable
		// that will hold an extracted value. To refer to a previously defined variable, enclose it in back ticks.
		// Conversely, a term that begins with an uppercase letter is assumed to be a type name.
		val (initLen, initSum) = (0,0)
		val llmo = listLen - 1
		val outList : List[Int] = List.unfold[Int, (Int, Int)]((initLen, initSum)) (tuple => {
			tuple match {

				case (`listLen`, _) => None
				case (sz, `listSum`) => Some(0, (sz + 1, listSum))
				case (`llmo`, sum) => Some(listSum - sum, (listLen, listSum))
				case (sz, sum) => Some(sz, (sz + 1, sum + sz))
			}
		})
		println(s"Another list of length=${listLen} and sum=${listSum}, contents=${outList}")
		val checklen = outList.length
		val checksum = outList.fold(0)(_ + _)
		println(s"Checklen = ${checklen}, Checksum = ${checksum}")
		outList
	}
}

trait MatchyExamples {
	def grabPieces[X](inSeq : Seq[X]) : Unit = {
		inSeq match {
			case x1 +: x2 +: many :+ x4 :+ x5 => println(s"Input of length=${inSeq.length}, matched x1=${x1}, x2=${x2}, many=${many}, x4=${x4}, x5=${x5}")
			case other => println(s"Input of length=${inSeq.length} matched other=${other}")
		}
	}
	def grabSillyItems[X](inSeq : Seq[X]) : Seq[String] = {
		inSeq map {
			// No parens in the if condition
			case xnum : Int if(xnum % 3 == 0) => s"Silly[${xnum} is divisible by 3]"
			case ynum : Int if(ynum % 2 == 1) => s"Silly[${ynum} is odd and not divisible by 3]"
			case other => s"NotSilly[${other}])"
		}
	}
	def decideLabel[X](inSeq : Seq[X]) : String = {
		inSeq match {
			case Seq(x1, _, x3, x4) => {s"Exactly4 [x1=${x1}, (x2 skipped), x3=${x3}, x4=${x4}]"}
			case Seq(x1, x2, x3, more @ _*) :+ last  => {s"4orMore [x1=${x1}, x2=${x2}, x3=${x3}, more=${more}, last=${last}]"}
			case Seq(x1, x2, rest @ _*) => {s"AtLeast2 [x1=${x1}, x2=${x2}, rest=${rest}]"}

			case other => s"OTHER[${other}]"
		}
	}
}

trait EvalTrampolineExamples {
	// These are from the "Scala with Cats" book:
	def factorial(n: BigInt): Eval[BigInt] = {
		if (n == 1) {
			Eval.now(n)
		} else {
			Eval.defer(factorial(n - 1).map(_ * n))
		}
	}
/*
	Wrapping accumulator value in an Eval
 */
	def foldRightEval[A, B](as: List[A], acc: Eval[B])
						   (fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
		as match {
			case head :: tail =>
				Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
			case Nil =>
				acc
		}
	}
	def foldRight[A, B](listOfA: List[A], accumB: B)(combiner: (A, B) => B): B = {
		val rsltEval: Eval[B] = foldRightEval(listOfA, Eval.now(accumB)) { (elem, evalAccumB) =>
			evalAccumB.map(combiner(elem, _))
		}
		rsltEval.value
	}
}

/*
@ can be used to bind a name to a successfully matched pattern, or subpattern.
Patterns can be used in pattern matching, the left hand side of the <- in for comprehensions, and in destructuring assigments.
 */

/*
General "fold" has more restrictive conditions than foldLeft or foldRight
 */