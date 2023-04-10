# gravax
Scratchpad Scala project containing mathematical computing experiments

* Streaming analytics for cloud environments
  * Functional wrappers for [Apache DataSketches](https://datasketches.apache.org), which implements stochastic streaming algos (Example: approx. histograms, approx. count-distinct)
  * Uses [ZIO-Streams](https://zio.dev/reference/stream/) for functional data streaming
  * Uses [ZIO-Lambda](https://zio.dev/zio-lambda/) to integrate with AWS cloud
* Several experiments using Typelevel libraries:  [FS-2](https://fs2.io), [Cats-Effect](https://typelevel.org/cats-effect), [Spire](https://typelevel.org/spire)
  * Streams of randomly generated data, e.g. triangles, with randomness isolated into effects.
  * Histograms (fed from FS2 streams) coded using immutable data structures
    * Monoidally combinable, allowing for parallel construction

  
