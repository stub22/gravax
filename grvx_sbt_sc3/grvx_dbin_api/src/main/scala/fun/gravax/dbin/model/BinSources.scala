package fun.gravax.dbin.model

import cats.effect.IO
import fs2.Stream
import fun.gravax.dbin.model.DbinModelTypes.BinTag


trait BinSource {
	// BinSource requires some effect type
	type SrcEff[X]
	def getBin(distribKeys : DistribKeys, binTag : BinTag) : SrcEff[Bin]

}

class RegularBinSource extends BinSource {
	override type SrcEff[X] = IO[X]

	override def getBin(distribKeys: DistribKeys, binTag: BinTag): IO[Bin] = {
		???
	}

	// Can wrap this into a Pipe to be called with .through on the tagStream.
	def getBinStream(distribKeys: DistribKeys, binTagStrm : Stream[IO, BinTag]) : Stream[IO, Bin] = ???
}