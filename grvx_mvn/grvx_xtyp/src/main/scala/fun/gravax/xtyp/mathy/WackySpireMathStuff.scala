package fun.gravax.xtyp.mathy

import spire.math.Complex

private trait WackySpireMathStuff

trait MakeSpireDoSomethinCool {

	def doSomeCmplxStf : Unit = {

		val a = 3.0
		val b = 5.0
		val c = Complex(a, b)
		println(s"Made c=${c}")
	}
}

/*
https://typelevel.org/spire/

Number Types
In addition to supporting all of Scala’s built-in number types, Spire introduces several new ones, all of which can be found in spire.math:

Natural unsigned, immutable, arbitrary precision integer
Rational fractions of integers with perfect precision
Algebraic lazily-computed, arbitrary precision algebraic numbers
Real computable real number implementation
Complex[A] complex numbers, points on the complex plane
Jet[A] N-dimensional dual numbers, for automatic differentiation
Quaternion[A] extension of complex numbers into 4D space
UByte through ULong value classes supporting unsigned operations
SafeLong fast, overflow-proof integer type
Number boxed type supporting a traditional numeric tower
Interval[A] arithmetic on open, closed, and unbound intervals
Polynomial[A] univariate (single-variable) polynomial expressions
Trilean value class supporting three-valued logic
FixedPoint fractions with Long numerator and implicit denominator (in extras)
Detailed treatment of these types can be found in the guide.

Type Classes
Spire provides type classes to support a wide range of unary and binary operations on numbers. The type classes are specialized, do no boxing, and use implicits to provide convenient infix syntax.

The general-purpose type classes can be found in spire.math and consist of:

Numeric[A] all number types, makes “best effort” to support operators
Fractional[A] fractional number types, where / is true division
Integral[A] integral number types, where / is floor division
Some of the general-purpose type classes are built in terms of a set of more fundamental type classes defined in spire.algebra. Many of these correspond to concepts from abstract algebra:

Eq[A] types that can be compared for equality
Order[A] types that can be compared and ordered
PartialOrder[A] types that can be compared for equality, and for which certain pairs are ordered
Semigroup[A] types with an associative binary operator |+|
Monoid[A] semigroups that have an identity element
Group[A] monoids that have an inverse operator
(Left/Right/)Action[P, G] left/right/ actions of semigroups/monoids/groups
Semiring[A] types that form semigroups under + and *
Rng[A] types that form a group under + and a semigroup under *
Rig[A] types that form monoids under + and *
Ring[A] types that form a group under + and a monoid under *
EuclideanRing[A] rings with quotients and remainders (euclidean division)
Field[A] euclidean rings with multiplicative inverses (reciprocals)
Signed[A] types that have a sign (negative, zero, positive)
NRoot[A] types that support k-roots, logs, and fractional powers
Module[V,R] types that form a left R-module
VectorSpace[V,F] types that form a vector space
NormedVectorSpace[V,F] types with an associated norm
InnerProductSpace[V,F] types with an inner product
MetricSpace[V,R] types with an associated metric
Trig[A] types that support trigonometric functions
Bool[A] types that form a Boolean algebra
Heyting[A] types that form a Heyting algebra
Variants of Semigroup/Monoid/Group/Action with partial operations are defined in the spire.algebra.partial subpackage.

In addition to the type classes themselves, spire.implicits defines many implicits which provide unary and infix operators for the type classes. The easiest way to use these is via a wildcard import of spire.implicits._.
 */