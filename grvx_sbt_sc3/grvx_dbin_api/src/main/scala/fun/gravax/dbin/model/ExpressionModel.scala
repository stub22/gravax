package fun.gravax.dbin.model

/**
 * An ExpressDist defines an explicit probability mass function as a math expression.
 * a) We can work with such an expression directly (to compute moments, marginals).
 * b) We can transform an ExpressDist into a BinDist or RegionDist
 * c) An ExpressDist usually requires some Params, which may be scalars, vectors, tensors.
 * These params may be defined in terms of other expressions
 * d) Some expressions are differentiable w.r.t. their params
 */
class ExpressionModel {

}

/**
 * An AlgoDist is defined by an algorithm.  For example, a uniform distribution.
 */

