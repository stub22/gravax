package fun.gravax.xtyp.histo

private trait HistoBaseStuff
/***
When processing a stream of samples, we can calculate the sample mean incrementally.
However, we cannot incrementally compute the exact variance or (in general) the exact median, unless we keep all
samples in memory.  (If the samples are well behaved we MAY be able to get the exact median from only a partial
sample history).

If we accumulate a histogram of samples, then we can compute approximate median, variance, and other population
summary statistics from this histogram.  The more bins we have in our histogram, the more accurate our estimates
will be.  However we may not know a priori how to arrange these bins to achieve the best tradeoff of accuracy
to storage space and compute load.

Histograms may be computed in parallel, with results combined via ordinary 'reduce' mechanics, when all the
bins of the histograms are laid out identically.



 Research areas as indicated by my open PDF tabs.
 */