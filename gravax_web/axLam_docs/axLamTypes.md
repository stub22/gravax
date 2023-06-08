This note itemizes the implementaion layers of AxLam implementation of [YaflSpec](../yafl_doc_drafts/YaflSpec_22A01a.pdf) for Scala 2.13.

Status 2023-06-07:  Pieces of AxLam implementation are scattered in the gravax.fun github [repository](https://github.com/stub22/gravax/). 
We are aligning that implementation with relevant platforms for math and interactive apps (e.g. Scala.js, Cats/ZIO, Feral serverless, Lean MathLib, ...). This note informs our alignment efforts and helps explain what AxLam is to our collaborators.

## AxLam-Core/Data
AxLam-Core/Data is a pure-value definition language (a type universe), implemented with a restricted subset of Scala 2.13 syntax.

The following ingredients may be used in defining AxLam-Data types. 

In all cases the fields and type parameters used by these core types may only refer to other AxLam-Data types.  This property may be checked during a serialization, up to the ordinary limits inherent in Scala type erasure.  Using common AxLam facilities helps to ensure Core-compliance at compile time as well.

#### 0) General Provisos:  No mutable data, no macros, no gratuitous fanciness
  * no vars, no futures, no mutable collections, no hidden mutable data
  * no macros
  * avoid unnecessary imports
  * _optional_ extra-tightness : may avoid implicits completely
 
#### 1) Use scala 'type' defintion operator, applied only to other AxLam-Core types

  * These are completely erased at runtime.  We use them for readability and compile-time type safety.
  * Unlike macros the abstract 'type' specs require no special compiler considerations.

#### 2) Product types, isomorphic to cartesian products:

  * Tuples (of other AxLam-Core/Data values)
  * Case classes (using only AxLam-Core/Data values)
    * fields, defs, vals, type-params of these case classes may only reference AxLam-Core values
    * _tight_ case classes have no contents besides their matchable constructor fields


#### 3) Sum types, equivalent to tagged unions
  * Option, Either (of AxLam-Core/Data types!)
  * All the related constructors:  Some, None, Left, Right
  * Sealed traits (which are usually implemented by neighboring case classes)
    * defs, vals, and type-params may only reference AxLam-Core types
    * when tightness is desirable, lazy vals should usually be avoided here
    * bonus-tightness:  no vals at all
    * mega-tightness : no defs either

#### 4) Immutable collections
  * Seq
  * Includes related constructors like Nil
  * Map[String, AnyAxLamCoreType]
    * only Strings may be used as map keys in AxLam-Core.  
    * This restriction ensures that data translation to/from JSON is trivial

#### 5) Primitive value types, inheriting from AnyRef.  All values are 'boxed'.

  * String
  * java.lang.Char, java.lang,Boolean
  * scala.math.BigInt, scala.math.BigDecimal
  * java.lang.Integer, java.lang.Long, java.lang.Short
  * java.lang.Float, java.lang.Double
    * These floating point types are included as a compromise between convenience and correctness.
    * BigDecimal or Spire types are preferable when correctness matters
  * [Spire](https://typelevel.org/spire/#number-types) numeric types
    * Edge case : allowing for implicits
    * The rest of Typelevel Pure (Cats) comes in in Axlam-Core/Func   

### Properties of AxLam Core/Data value universe
  * Composable
    * Algebra of type construction follows modern contours of elementary finite category theory and constructive type theory.
    * See references on type universes, sum and product types.
  * Finite extent when fully instantiated
    * Serializable to/from JSON, with known issues re: numbers, verbosity.
    * Mappable into off-heap storage 
  * Expect reliable behavior (except for numbers) across platforms including Scala-JVM, Scala.js, Scala-Native
  * Small classloader footprint
  * .equals and .hashCode are mostly trivial, and provided by platform (except for equality on numbers)
  * Generally (but not easily proven to be) pure when compliant, aside from RAM consumption, edgy number effects.
  * Partly verifiable (as being "AxLam-Core-compliant") at compile time, and partly at runtime.  

### Weaknesses
  * We cannot cleanly define a Scala inheritance hierarchy of all Core/Data types, without introducing wrappers.  
    * Interpreter code must often match against all subtypes of AnyRef.
  * Our algebra of parameterized types is not easy to categorize.
  * We are limited in ability to express dependent types or refinement types.
  * No explicit proof capabilities.

### Not included in AxLam-Core/Data
  * No throwables
  * No functions (but see next section on AxLam-Core/Func)
  * No references to Unit
  * No references to Nothing
  * No effects
  * No streams
  * No RNGs
  * No "vars" or other explicit mutability

### AxLam-Core/Func (pure)
This type universe includes 
1) All of AxLam-Core/Data 
2) "Haskell-like" one argument pure functions which accept and produce AxLam-Core instances
  * `scala.Function1[-In,+Out]` where In and Out are in AxLam-Core
  * Clean interop with familiar functional operations:  map, flatMap, fold, filter, isEmpty
  * Any functions which are recursive should be @tailrec  
3) All the type construction mechanisms of AxLam-Core apply, so we may compliantly write:
```
val goodCoreThing : Map[String, Function1[Seq[Function1[Long,BigDecimal]], String]] 
```

#### Properties of AxLam Core/Func universe
  * Pure(-ish, repeating caveats from above)
  * Functions are limited to the most compiler-friendly, portable and optimizable kinds
  * Not generally serializable to JSON.
  * .hashcode and .equals are OK 

## AxLam-Full/Func (pure)
AxLam-Full allows us to use general pure functions and streams.  As a type-universe, it includes:

1) All of AxLam-Core

2) Function[] instances (whose arguments are in AxLam-Full/Func)

3) Typelevel Pure features

3) Pure lazy functional streams
  * fs2.Stream[Pure, AnyAxLamFullType]
  * zio.UStream[AnyAxLamFullType]
4) Traits and classes that use only the above ingredients, and each other

## AxLam-More/Code (impure)

A restricted set of ordinary impure capabilities supporting execution

----

Our original specification from YaflSpec-2022-Jan

YaflSpec v0.1 outlines four layers of specification, in alphabetical order of inclusion 

YaflSpec-Core (YaflCore) defines a model of a limited space of algebraic datatypes, with limited induction (restricted self-inclusion).
YaflCore includes a single category of one-arg pure function types which may only process ordinary data, and may not explicitly refer to functions or types as data. (No lambda arguments, no closures, …).

YaflSpec-Full (YaflFull) defines a limited model of pure functions with arguments and results drawn from (YaflCore | YaflFull) including limited use of functions and types as explicit data items. 

YaflSpec-More (YaflMore) defines additional useful but less-common datatypes including binary data and data artifacts designed to support execution. Functions using YaflMore types are more limited, and may not be treated as first-class data themselves. 

YaflSpec-Proof (YaflProof) allows for specification of proofs regarding types in the lower layers, to strengthen our expectations about Yafl functions and types. 

