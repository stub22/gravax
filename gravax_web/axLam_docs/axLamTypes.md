This dry doc itemizes the basics of how AxLam implements YaflSpec for Scala 2.13.

Pieces of implementation are scattered in the gravax.fun repository.

## AxLam-Core/Data
AxLam-Core/Data is a pure-value definition language (a type universe), implemented with a restricted subset of Scala 2.13 syntax.

The following ingredients may be used in defining AxLam-Data types. 

In all cases the fields and type parameters used by these core types may only refer to other AxLam-Data types.  This property may be checked during a serialization, up to the ordinary limits inherent in Scala type erasure.  Using common AxLam facilities helps to ensure Core-compliance at compile time as well.

0) General Proviso:  No mutable data
  * no vars, no futures, no mutable collections, no hidden mutable data
 
1) Scala 'type' operator, applied only to other AxLam-Core types
  *These are completely erased at runtime.  We use them only as syntactic sugar. They are very useful as an organizing syntax.  
  *Unlike macros they require no special considerations.

2) Product types, isomorphic to cartesian products:

  * Tuples (of other AxLam-Core value)
  * Case classes (with field containing only AxLam-Core values)
    * fields, defs, vals of these case classes may only reference AxLam-Core values
    * _tight_ case classes have no contents besides their matchable constructor fields

3) Sum types, equivalent to tagged unions
  * Option, Either (of Axlam Core types!)
  * All the related constructors:  Some, None, Left, Right
  * Sealed traits (which are usually implemented by neighboring case classes)
  * defs and vals may only reference AxLam-Core types
    * when tightness is desirable, lazy vals should usually be avoided here
    * bonus-tightness:  no vals at all
    * mega-tightness : no defs either
  

4) Immutable collections
  * Seq
  * Includes related constructors like Nil
  * Map[String, AnyAxLamCoreType]
    * only Strings may be used as map keys in AxLam-Core.  
    * This restriction ensures that data translation to/from JSON is trivial

5) Primitive value types, inheriting from AnyRef.  All values are 'boxed'.

  * String
  * java.lang.Char, java.lang,Boolean
  * scala.math.BigInt, scala.math.BigDecimal
  * java.lang.Integer, java.lang.Long, java.lang.Short
  * java.lang.Float, java.lang.Double
    * These floating point types are included as a compromise between convenience and correctness.
    * BigDecimal or Spire types are preferable when correctness matters
  * [Spire](https://typelevel.org/spire/#number-types) numeric types

### Properties of AxLam Core values

  * Composable
  * Serializable to/from JSON, with known issues re: numbers, verbosity.
  * 'Pure' when compliant, aside from RAM/heap consumption
  * Finite extent (i.e. heap usage) when fully instantiated
  * Partly verifiable (as being "AxLam-Core-compliant") at compile time, and partly at runtime.  
  * Reliable behavior (except for numbers) across platforms.
  * Small classloader footprint
  * .equals and .hashCode are mostly trivial, and provided by platform (except for equality on numbers)

### Weaknesses
We cannot cleanly define a Scala inheritance hierarchy of all Core/Data types, without introducing wrappers.  
Our algebra of parameterized types is not easy to categorize.

### Not included
  * No throwables
  * No functions
  * No references to Unit
  * No references to Nothing
  * No effects
  * No streams
  * No RNGs
  * No "vars" or other explicit mutability

### AxLam-Core/Func
This type universe includes 
1) All of AxLam-Core/Data 
2) "Haskell-like" one argument pure functions which accept and produce AxLam-Core instances
  * Clean interop with familiar functional operations:  map, flatMap, fold, filter, isEmpty

## AxLam-Full/Func
AxLam-Full allows us to build general pure functions and streams.  As a type-universe, it includes:

1) All of AxLam-Core

2) Function[] instances (whose arguments are in AxLam-Full/Func)

3) Pure lazy functional streams
  * fs2.Stream[Pure, AnyAxLamFullType]
  * zio.UStream[AnyAxLamFullType]
4) Traits and classes that use only the above ingredients, and each other

## AxLam-More/Code

A restricted set of ordinary impure capabilities supporting execution

----

Our original specification from YaflSpec-2022-Jan

YaflSpec v0.1 outlines four layers of specification, in alphabetical order of inclusion 

YaflSpec-Core (YaflCore) defines a model of a limited space of algebraic datatypes, with limited induction (restricted self-inclusion).
YaflCore includes a single category of one-arg pure function types which may only process ordinary data, and may not explicitly refer to functions or types as data. (No lambda arguments, no closures, …).

YaflSpec-Full (YaflFull) defines a limited model of pure functions with arguments and results drawn from (YaflCore | YaflFull) including limited use of functions and types as explicit data items. 

YaflSpec-More (YaflMore) defines additional useful but less-common datatypes including binary data and data artifacts designed to support execution. Functions using YaflMore types are more limited, and may not be treated as first-class data themselves. 

YaflSpec-Proof (YaflProof) allows for specification of proofs regarding types in the lower layers, to strengthen our expectations about Yafl functions and types. 

