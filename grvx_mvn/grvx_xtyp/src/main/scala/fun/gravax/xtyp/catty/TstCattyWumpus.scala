package fun.gravax.xtyp.catty

import fun.gravax.xtyp.mathy.MakeSpireDoSomethinCool

private trait TstCattyWumpus // SourceFileNameMarkerTrait

/*
User models should  be represented as typed instance data, not as Scala code, nor embedded-DSL code, nor text scripts.

A user model describes r-values using function composition where functions are either:
  1) references to named funcs (pure, total) supplied by context
  2) morphisms explicitly composed thru (functorial) algebraic operations equivalent to
    products (tuples/records),
    alternative-sums (cases),
    optics (projections/fields/paths/lenses)
    associative monoidal operations (e.g. string-concat, multiply)

Each r-value composition must be describable as a single finite record of data,
which we may treat as a term in an impredicative universe of (finite) data-only types.

There are no recursive functions or inductive definitions in this impredicative universe.
This impred. universe is compatible with first order propositional logic.
Call this universe #AxLamIFO where IFO = Impredicative First Order.
Thus

User identifies an l-values as typed context frames (capturing a single r-value of compatible type).

An expnded r-value specifies
1) Its imported names (URIs of types and pure-IFO-functions), with optional local alias names
2) Optional named typed parameters, where each type is a dependent-typed-expression
3) Optional local constants
4) A single composed expression as a pointed labeled DAG (directed acyclic graph).
"Pointed" indicates expression root.
A composed expression has the form:
     applyFunc(funcRef, singleInputExpr)
     // recordTypeRef may apply any predicate to fields (/tuple-slots).
   | buildRecord([recordTypeRef], [nameA] exprA, [nameB] exprB, ...)
   // "Loose" = One or more of (predA, predB...) must hold (provably,totally), and they should be tried in the order given
   | chooseAltLoose ((predA exprA), (predB exprB), (predC exprC))
   // "Strict" = Exactly one of (predA, predB...) must hold (provably), and they may be tried in parallel, with
   // confidence that exactly one will succeed.
   | chooseAltStrict ((predA exprA), (predB exprB), (predC exprC))

	// exprs must *all* be of elemType (or subTypes)
   | buildFinList([elemTypeRef], exprA, ...
   // the function defines a nested r-value context where the single input-param is available
   | defineFunc (inputTypeExpr, nested-r-valueExpr)
   | defineType (typeDefExpr)

   // Note that we do not allow definition of function types by this mechanism.
   typeDefExpr = recordTypeDefExpr | altTypeDefExpr | finListTypeDefExpr

   recordTypeDefExpr = (unordered-collection-of (fieldName, typeExpr), outerPredicate)

   A finiteSet is defined as an equivalence-class of finiteLists.
   A kvPair is defined as a record type with two fields ("key" and "value") and no outer predicate.
      Keys must supply equality.

   A dictionary is defined as a finiteSet of keys and a mapping from key to value.  It may be constructed from a list
   of kvPairs, with understanding that order of keys is irrelevant, and keys must not be repeated.

funcRef as used in funcApply may be
	ref-to-named-func

	map/flatMap style application
 */
object RunCattyWumpus {
	def main(args: Array[String]): Unit = {
		goDoStuff
	}
	def goDoStuff : Unit = {
		println ("RunCattyWumpus doing stuff yay")
		val nmstf = new NuttyMonoids {}
		val xtxt = nmstf.someSemigroupAntics
		println(s"semigroup magic yields xtxt=${xtxt}")

		val scs = new MakeSpireDoSomethinCool{}
		scs.doSomeCmplxStf
	}
}
