package fun.gravax.gravnum

private trait YaflSpecStuff

/*
This scala file presents a definition of the core concepts and names of
Yafl == YAFL == Yet Another Func(tional) Lang(uage)

Intended as an abstract pure-function spec compatible among Scala, Idris, Haskell, Agda

Yafl intends to be 100% pure as a specification of input-output functions in the mathematical sense.
Yafl functions may be decidable or not.

Particular programs may be proven decidable, and might be run undecidably under experimental conditions.

Implementation of Yafl requires implementation of (some part of ) the Yafl type system within some system.

Yafl-Full is defined in theory as the space of pure typed 1-arg functions, where arg + result types have
some common bound.  In scala we may sketch the primary Yafl function type like so, but note we get no
guarantee of purity from this trait.

 */

trait YaflPureFunc01[YDat, YIn <: YDat, YOut <: YDat] extends Function1[YIn, YOut] {
	// Implementor must supply a pureEnoughCalc which is sufficiently trustworthy for system using Yafl.
	// This allows arbitrary system integration with all the attendant risks (and sometimes practical benefits).
	// Note that by using explicit functions-as-data in a Yafl-aligned language like axLam, functions are
	// constructed which are known to be pure, and are subject to further proofs (although mistakes in the space
	// of "known" and "proven" are also possible).
	def pureEnoughCalc(in : YIn) : YOut

	override def apply(in: YIn): YOut = pureEnoughCalc(in)
}
/*
Here we focus on Yafl-Core which supports composition of functions
over strong type of immutable data.
// YaflCore is defined as functions like YaflPureFunc01[YaflCoreDat]
All YaflCoreDat is immutable

grounded in the3se three YaflPrimDat types
   YaflInt, YaflRat, YaflTxt
which are all primitive subtypes of YDat.
The composite subtypes of YCoreDat are derived from these a limited , roughly
  1) Records/Cartesian-Products (N-tuples of input types)
  2) Sums of alternatives, like Either
  3) List
axLam : Yafl

axLam is an YaflFunc instance  triangle: RDF, JSON, Scala

Limited spaces of YaflFuncs are constructed as axLam func descriptions
 as subtypes of YaflFunc01 which are one-arg functions.
Multi arg functions YaflFunc02... are sugar-only in our axLam impl.

Actual impls of AxLam funcs are of two kinds:
1) Authored functions describing typed Lambda functions of Ydat
2) Coded runtime funcs implementing equivalent of YaflFunc01 (Java/Scala or LLVM or ...) in a way that is "good enough"
for the mission.  Goodness of the functions is the subject of proofs.

 */
// Here is a taxonomy of YaflCoreDat types as scala traits without type parameters nor methods.
// These are design markers aligned to YaflCore specification.
// An instance of YaflCoreDat is an item of YaflCore data.
// Note that YaflCoreDat does not directly support URIs at this level.
// Instead URIs may be defined (constructively?) in a specific more limited, higher level authoring language, e.g. axLam
sealed trait YaflCoreDat
trait YaflPrimDat extends YaflCoreDat
trait YaflTxtDat
trait YaflNumDat
trait YaflRatNum extends YaflNumDat
trait YaflIntNum extends YaflRatNum

// YaflCore offers 3 composite types : Cartesian Product, Alternative Sum, Finite List
trait YaflComposite extends YaflCoreDat

// CartProd =
// Dependent-typed cartesian product, equivalent to Tuple-N over types T1, T2 .. TN
trait YaflCartProd extends YaflComposite

// AltSum =
// Dependent-typed sum, equivalent to some N-chain of Eithers over types T1, T2 .. TN
trait YaflAltSum extends YaflComposite

// Above two types are sufficient for algebraic composition purposes of what we may call finite intensional types.
// As a design choice we include one further redundant type, Finite list.
// FinList is a dependent-typed finite list of items
// where 1) all items come from some fixed type T <: YaflCoreDat
// and  2) Yafllength is included in type.
// Clarifying the "redundancy":  Algebraically the YaflFinList type is equivalent to a Sum over different length Products
trait YaflFinList extends YaflComposite

// Then punchline is to implement YaflPureFunc01 over YaflCoreDat

trait YaflCoreFunc[In <: YaflCoreDat, Out <: YaflCoreDat] extends YaflPureFunc01[YaflCoreDat, In, Out]


// YaflCore is defined as functions like YaflPureFunc01[YaflCoreDat]

/* Quoting from scala package of 2.13.7 as decompiled by IDEA on 21-12-31

trait Function1[@scala.specialized -T1, @scala.specialized +R] extends scala.AnyRef {
  def apply(v1 : T1) : R
  @scala.annotation.unspecialized
  def compose[A](g : scala.Function1[A, T1]) : scala.Function1[A, R] = { /* compiled code */ }
  @scala.annotation.unspecialized
  def andThen[A](g : scala.Function1[R, A]) : scala.Function1[T1, A] = { /* compiled code */ }
  override def toString() : _root_.scala.Predef.String = { /* compiled code */ }
}
object Function1 extends scala.AnyRef {
  implicit final class UnliftOps[A, B] private[Function1] (f : scala.Function1[A, scala.Option[B]]) extends scala.AnyVal {
    def unlift : scala.PartialFunction[A, B] = { /* compiled code */ }
  }
}
 */
