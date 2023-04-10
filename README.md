# gravax
Scratchpad Scala project containing mathematical computing experiments, used as exploratory use cases for "AxLam" methodology.

* Streaming analytics for cloud environments
  * Functional wrappers for [Apache DataSketches](https://datasketches.apache.org), which provides non-functional stochastic streaming algos (using mutable Java data structs)
  * Example: Approximate histograms for an input stream of samples, computed in parallel
  * Uses [ZIO-Streams](https://zio.dev/reference/stream/) for functional data streaming
  * Uses [ZIO-Lambda](https://zio.dev/zio-lambda/) to integrate with AWS cloud
* Several experiments using Typelevel libraries:  [FS-2](https://fs2.io), [Cats-Effect](https://typelevel.org/cats-effect), [Spire](https://typelevel.org/spire)
  * Streams of randomly generated data, e.g. triangles, with randomness isolated into effects.
  * Exact histograms (fed from FS2 streams) coded using immutable data structures
    * Monoidally combinable, allowing for parallel construction

  
