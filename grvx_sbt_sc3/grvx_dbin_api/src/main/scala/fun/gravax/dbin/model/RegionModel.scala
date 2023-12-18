package fun.gravax.dbin.model

private trait RegionModel

/***
 * An N-region is a subset of an N-dimensional event space.
 * A region distribution describes, for a set of *disjoint* regions:
 *   a) Probability of event in each region
 *   b) Statistics of events within each region
 *   c) Available sub-regions for each region
 *
 *   The difference from binned distributions is that
 *   a) bins do not have boundaries, they only have centers.
 *   b) regions are disjoint, while bins may overlap.
 *
 *   We are interested in defining morphisms between binDists and regionDists.
 *
 *   Regions may be easily turned into bins, one for one.
 *   The bins can then be refined using clustering.
 *
 *   However, a translation from bins to regions is more challenging.
 *   Each region receives *some* mass from *all* bins.
 *   Choosing the best boundaries between regions could be a task for SVM learning.
 *
 *   Raw event data may be interpreted as a fine-grained Region-Dist.  Seems natural to aggregate into larger regions.
 *   Or, raw event data can instead be interpreted as tiny bins in need of clustering.
 *
 */
trait RegionInfo {

}
