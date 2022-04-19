package fun.gravax.xtyp.catty

import cats.arrow.FunctionK
import cats.data.Kleisli
import cats.{Applicative, Eval, Functor, Id, Monad, Monoid}
import cats.instances.option._
import cats.instances.list._


private trait ExploreCatsApi

trait HmmOkCats {
 	def mkMonadExamples: Unit = {
		val monadForOpt = Monad[Option]
		val opt1: Option[Int] = monadForOpt.pure(3)
		// opt1: Option[Int] = Some(3)
		val opt2 = monadForOpt.flatMap(opt1)(a => Some(a + 2))
		// opt2: Option[Int] = Some(5)
		val opt3 = monadForOpt.map(opt2)(a => 100 * a)
		// opt3: Option[Int] = Some(500)

		println(s"opt1=${opt1}\nopt2=${opt2}\nopt3=${opt3}")
		val list1 = Monad[List].pure(3)
		// list1: List[Int] = List(3)
		println(s"list1=${list1}")

		val ewk = new ExWithKleisli {}
		val ee: Kleisli[Id, Unit, Boolean] = ewk.mkExamplePipeK(250, math.Pi)
		val r: Id[Boolean] = ee.apply()
		println(s"examplePipeK.apply = ${r}")

		ewk.goFuncK()
		doMoreKleisli
	}
	def doMoreKleisli : Unit = {
		val ewk = new ExWithKleisli {}
		val ki = ewk.mkKleisliExForId
		val ko = ewk.mkKleisliExForOption
		val kl = ewk.mkKleisliExForList
		val ke = ewk.mkKleisliExForEval
		println(s"MoreKleisli ki.apply=${ki.apply()}")
		println(s"MoreKleisli ko.apply=${ko.apply()}")
		println(s"MoreKleisli kl.apply=${kl.apply()}")
		println(s"MoreKleisli ke.apply=${ke.apply()}, ke.apply.value = ${ke.apply().value}")
	}

}
trait ExWithKleisli {
	def likeEx : Unit = {
		val r = scala.util.Random

		val generate: Unit => Int = _ => r.nextInt(100)
		val process: Int => Int = v => (v * math.Pi).toInt
		val save: Int => Boolean = _ => true

		val combine_2: Unit => Boolean = save compose process compose generate
		println(s"Result 2 is: ${combine_2()}")
		//this is a bit difficult to read too as we are used to read from left to right

		//andThen version
		val combine_3: Unit => Boolean = generate andThen process andThen save
		println(s"Result 3 is: ${combine_3()}")
	}
	// When we know our kosher Monad type and we can bring its
	def mkExamplePipeK(maxNum : Int, piConst : Double) : Kleisli[Id, Unit, Boolean] = {
		val r = scala.util.Random

		val mid = Monad[Id] // Using implicits for our chosen kosher Monad type: Id.
		// Could have been Option, Either, List, or other Functors that Cats provides pre-enriched.
		// But we have to PICK one, or else use a FreeMonad (requires cats-free import)
		val generate: Unit => Id[Int] = _ => mid.pure(r.nextInt(maxNum))
		val process: Int => Id[Double] = num => mid.pure(num * piConst)
		val save: Double => Id[Boolean] = number => mid.pure(true)

		val gk: Kleisli[Id, Unit, Int] = Kleisli(generate)
		val gp = Kleisli(process)
		val gs = Kleisli(save)

		val chn: Kleisli[Id, Unit, Boolean] = gk andThen gp andThen gs
		chn
	}
	def wow : Unit = {
		val a: Id[Int] = Monad[Id].pure(3)
		// a: Id[Int] = 3
		val b = Monad[Id].flatMap(a)(_ + 1)
		// b: Id[Int] = 4

		val mndForId: Monad[Id] = Monad[Id]

		val monadForOpt = Monad[Option]
		val opt1: Option[Int] = monadForOpt.pure(3)

		import cats.syntax.functor._ // for map
		import cats.syntax.flatMap._ // for flatMap
	}
	def butWeDontHaveFreeMonadInScope[X[_]]() : Kleisli[X, Unit, Boolean] = {
		val r = scala.util.Random
		// val mid = Monad[X] // Could not find an instance of Monad for X
		???
		// cats-free "allows you to build a monad from any Functor"
		// https://typelevel.org/cats/datatypes/freemonad.html
	}
	// Build a composed pipeline for an arbitrary Monad
	def passItIn[X[_]](m4x : Monad[X], maxNum : Int, piConst : Double) : Kleisli[X, Unit, Double] = {
		val r = scala.util.Random

		// Our explicit monad reference gets used to make result-wrappers with .pure
		val generate: Unit => X[Int] = _ => m4x.pure(r.nextInt(maxNum))
		val process: Int => X[Double] = num => m4x.pure(num * piConst)
		val half: Double => X[Double] = number => m4x.pure(number / 2.0)

		val kg = Kleisli(generate)
		val kp = Kleisli(process)
		val ks = Kleisli(half)

		// Our explicit monad is used to provide FlatMap[X] for compose / andThen
		val c: Kleisli[X, Int, Double] = ks.compose(kp)(m4x)
		val yay: Kleisli[X, Unit, Double] = kg.andThen(c)(m4x)
		yay
	}
	def mkKleisliExForId : Kleisli[Id, Unit, Double] =  passItIn(Monad[Id], 333, 3.14)
	def mkKleisliExForOption : Kleisli[Option, Unit, Double] =  passItIn(Monad[Option], 333, 3.14)
	def mkKleisliExForList : Kleisli[List, Unit, Double] =  passItIn(Monad[List], 333, 3.14)
	def mkKleisliExForEval : Kleisli[Eval, Unit, Double] =  passItIn(Monad[Eval], 333, 3.14)

	def wrapInKleisli[X[_], A, B](someFuncProducingMonadicResult : Function1[A, X[B]]): Kleisli[X,A,B]  = {
		Kleisli(someFuncProducingMonadicResult)
	}
	def mkMonadProducer[X[_], A, B](monadInst : Monad[X], someFunc : Function1[A, B]): Function1[A, X[B]] = {
		// Wrap someFunc into another function that also achieves the monad's effect
		val monFunc: A => X[B] = a => monadInst.pure(someFunc(a))
		monFunc
	}

	def makeAndWrap[X[_], A, B](m4x : Monad[X], someFunc : Function1[A, B]) : Kleisli[X,A,B] = {
		val monProd = mkMonadProducer(m4x, someFunc)
		val kleiWrap = wrapInKleisli(monProd)
		kleiWrap
	}

	/*
	def whatAboutEval : Unit = {
		val m4e = Monad[Eval]
		val k4e = passItIn(m4e, 555, 3.1415)
		val e: Eval[Boolean] = k4e.apply()

	}
	*/

		/*
We could use generics to make this a bit more general:

def first[A](l: List[A]): Option[A] = l.headOption
But how would we represent this new first method as a =>/Function1 value?
We are looking for something like a type of List[A] => Option[A] forAll A, but this isn’t valid scala syntax.
Function1 isn’t quite the right fit, because its apply method doesn’t take a generic type parameter.

Higher Kinds to the Rescue
It turns out that we can represent our universal List to Option transformation with something that looks a bit like
Function1 but that adds a type parameter to the apply method and utilizes higher kinds:

trait MyFunctionK[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
Cats provides this type as FunctionK
	// FunctionK supplies  .apply, .compose, .andThen, .or,
*/
	def mkHeadOpterFuncK() : FunctionK[List, Option] = {
		val first: FunctionK[List, Option] = new FunctionK[List, Option] {
			def apply[A](l: List[A]): Option[A] = l.headOption
		}
		first
	}
	def goFuncK() : Unit = { 
		val hofk = mkHeadOpterFuncK()
		val someList: List[Int] = List(9, 8, 7)
		val ho : Option[Int] = hofk.apply(someList)
		println(s"after funcK we got ho=${ho}")

		val nobodyCaresButFKid: FunctionK[Option, Option] = FunctionK.id[Option]
		val samer = nobodyCaresButFKid.apply(ho)
		println(s"Now samer=${samer}")
	}

	def functionKid[X[_]]: FunctionK[X, X] = FunctionK.id[X]
}
trait ChoiceActXforms[ChoiceSet, ActSeq] {
	def useChoiceToInformAction (choices : ChoiceSet, actionSeqTemplate : ActSeq) : Either[String, ActSeq] = {
		???
	}
	def dok(chc : ChoiceSet) : Unit = {
		val xformActSeqFunc: ActSeq => Id[ActSeq] = mkPretendActSeqXformer()

		val chooserK = Kleisli(xformActSeqFunc)

		val txtCountFunc : String => Id[Int] = ???
		val txtCountK = Kleisli(txtCountFunc)
	}
	def mkPretendActSeqXformer() : Function1[ActSeq, ActSeq] = {
		???
	}

		/*
The problem with all the above examples is that we need to match the inputs of one function with
the outputs of another to make this all work. If some of the outputs will be a wrapper around some
type (e.g. Future or Cats IO) we will get into trouble quickly.
Let’s wrap some values into something useful

While we have only talked about flatMap above, monadic behaviour is for‐
mally captured in two operations:

• pure, of type A => F[A];
• flatMap¹, of type (F[A], A => F[B]) => F[B].

pure abstracts over constructors, providing a way to create a new monadic
context from a plain value. flatMap provides the sequencing step we have
already discussed, extracting the value from a context and generating the next
context in the sequence.

		 */


}