package fun.gravax.aa.yafl.core

private trait YaflSpecStuff

/*
This scala file presents a definition of the core concepts and names of
Yafl == YAFL == Yet Another Func(tional) Lang(uage)

YaflFunc == YaflFull is defined in theory as the space of pure typed 1-arg functions, where arg + result types have
some common bound.  In scala we may sketch the primary Yafl function type like so, but note we get no
guarantee of purity from this trait.

Yafl-Func allows for 1st class functions as arguments and results.
Yafl-Core does not.
YaflPureFunc01 is in Yafl-Func.
 */

trait YaflPureFunc01[YDat, YIn <: YDat, YOut <: YDat] extends Function1[YIn, YOut] {
	/*
	 Implementor must supply a pureEnoughCalc which is sufficiently trustworthy for system using Yafl.
	 This allows arbitrary system integration with all the attendant risks (and sometimes practical benefits).
	 Note that by using explicit functions-as-data in a Yafl-aligned language like axLam, functions are
	 constructed which are known to be pure, and are subject to further proofs (although mistakes in the space
	 of "known" and "proven" are also possible).
	 */
	def pureEnoughCalc(in : YIn) : YOut

	override def apply(in: YIn): YOut = pureEnoughCalc(in)
}
/*
All YaflCoreDat is immutable.
Primitive scalar types are derived from Text and Number.
The composite subtypes of YCoreDat are derived from these a limited , roughly
  1) Records/Cartesian-Products (N-tuples of input types)
  2) Sums of alternatives, like Either
  3) Finite List of type-bounded length.

Here is a taxonomy of YaflCoreDat types as scala traits without type parameters nor methods.
These are design markers aligned to YaflCore specification.
(As of 22-01-03, these *are* the only specifcation)
An instance of YaflCoreDat is an item of YaflCore data.
Note that YaflCoreDat does not directly support URIs at this level.
Instead URIs may be defined (constructively?) in a specific more limited,
higher level authoring language, e.g. axLam
 */
sealed trait YaflCoreDat
trait YaflPrimDat extends YaflCoreDat
trait YaflTxtDat
trait YaflNumDat // In yaflCore is always a rational

trait YaflRatNum extends YaflNumDat // Rational number which may reduce fraction, like 8/4 : PosRat == 4/2 : PosRat == 2 : PosInt

trait YaflIntNum extends YaflRatNum

// YaflCore offers 3 composite types : Cartesian Product, Alternative Sum, Finite List
trait YaflComposite extends YaflCoreDat

// CartProd =
// Dependent-typed cartesian product, equivalent to Tuple-N over types T1, T2 .. TN
trait YaflCartProd extends YaflComposite

// AltSum =
// Dependent-typed sum, equivalent to some N-chain of Eithers over types T1, T2 .. TN
trait YaflAltSum extends YaflComposite

/*
Above two types are sufficient for algebraic composition purposes of what we may call finite intensional types.
As a design choice we include one further dependent type, Finite list.
FinList is a dependent-typed finite list of items of known length (equivalent to a Product)
where 1) all items come from some fixed type T <: YaflCoreDat
and  2) yaflList Len (a number term) is included in type of the FinList (see Barendregt lambda cube)

Clarifying the "redundancy":  Algebraically the YaflFinList type is equivalent to a Sum over different length Products
*/

trait YaflFinList extends YaflComposite

/*
Now our punchline and conclusion to YaflCore in Scala is to define this single-arg YaflCoreFunc.
YaflCore layer does not allow functions or types to themselves be part of YaflCoreDat.
No functions in the In or Out here!
 */

trait YaflCoreFunc[In <: YaflCoreDat, Out <: YaflCoreDat] extends YaflPureFunc01[YaflCoreDat, In, Out]
