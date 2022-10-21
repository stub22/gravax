package fun.gravax.xtyp.catstack

private trait AboutMonoids

/*
Quotes from Wikipedia, Cats docs, Bartosz Milewski, and RockTheJVM blog articles:

WP has separate pages for Monoid in algebra and cat theory

In abstract algebra, a branch of mathematics, a monoid is a set equipped with an associative binary operation and
an identity element.

For example, the functions from a set into itself form a monoid with respect to function composition.
More generally, in category theory, the morphisms of an object to itself form a monoid, and, conversely, a monoid may be viewed as a category with a single object.

"""A monoid is, essentially, the same thing as a category with a single object.
More precisely, given a monoid (M, •), one can construct a small category with only one object and
whose morphisms are the elements of M. The composition of morphisms is given by the monoid operation •."""
BM in Dao of FP says:  "A category with a single object is called a monoid. The combining operation is the composition of arrows and the unit is the identity arrow."


"the free monoid on a set is the monoid whose elements are all the finite sequences (or strings) of zero or more elements from that set"
 */
