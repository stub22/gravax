package fun.gravax.distrib.gen

import fun.gravax.distrib.struct.{BinMassInfo, BinMeatInfo, BinTypes}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, UIO, Random => ZRandom}

import java.math.MathContext
import scala.collection.immutable.{Map => SMap}

private trait GenMeatStuff

// These params are applied to value generation in the BASE level of bins.
// The virtual bins are aggregated from those base.
trait MeatGenParams {
	val (meanMin, meanMax) = (BigDecimal("-1.0"), BigDecimal("1.0"))
	val (vrMin, vrMax) = (BigDecimal("0.0"), BigDecimal("0.5"))
	val (myKeyLen, myNumKeys) = (3, 8)
}
trait MassGenParams {
	val (massMin, massMax) = (BigDecimal("1.0"), BigDecimal("1000.0"))
}

trait GenMeatAndMassData extends KnowsGenTypes {

	def gaussianBD(zrnd : ZRandom, mathCtx : MathContext)(mean : BigDecimal, stdDev : BigDecimal): UIO[BigDecimal] = {
		val stdNumEff: UIO[Double] = zrnd.nextGaussian
		stdNumEff.map(num => {
			// https://blogs.oracle.com/javamagazine/post/four-common-pitfalls-of-the-bigdecimal-class-and-how-to-avoid-them
			val stdBD = BigDecimal(num, mathCtx)
			val scaledAndShiftedBD = stdBD.*(stdDev).+(mean)
			scaledAndShiftedBD
		})
	}

	def truncGaussBD(zrnd : ZRandom, mathCtx : MathContext)(min : BigDecimal, max : BigDecimal): UIO[BigDecimal] = {
		val diff = max.-(min)
		val mean = min.+(diff./(2))
		val stdDev = diff./(3)
		val gbdEff = gaussianBD(zrnd, mathCtx)(mean, stdDev)
		val bgbdEff : UIO[BigDecimal]  = gbdEff.repeatWhile(rslt => (rslt.compareTo(min) < 0 || rslt.compareTo(max) > 0))
		bgbdEff
	}

	def genRandStatEntry(zrnd : ZRandom, mathCtx : MathContext)(ekey : EntryKey, meatGenPrms : MeatGenParams) : UIO[StatEntry] = {
		for {
			mean <- truncGaussBD(zrnd, mathCtx)(meatGenPrms.meanMin, meatGenPrms.meanMax)
			vr <- truncGaussBD(zrnd, mathCtx)(meatGenPrms.vrMin, meatGenPrms.vrMax)
		} yield (ekey, mean, vr)
	}


	def genRandStatMap(zrnd : ZRandom, mathCtx : MathContext)(keys : Iterable[EntryKey], meatGenPrms : MeatGenParams) : UIO[StatMap] = {
		val kstrm: UStream[EntryKey] = ZStream.fromIterable(keys)
		// This could be done in fewer lines using ZSink.collectAllToMap
		val stStrm: UStream[(EntryKey, StatEntry)] = kstrm.flatMap(key => {
			val steOp  = genRandStatEntry(zrnd, mathCtx)(key, meatGenPrms)
			val tupOp = steOp.map(statEnt => (statEnt._1, statEnt))
			ZStream.fromZIO(tupOp)
		})
		// val chunkMkrSink = ZSink.collectAll[(BinTypes.EntryKey, BinTypes.StatEntry)]
		val stChunk_op: UIO[Chunk[(EntryKey, StatEntry)]] = stStrm.runCollect
		val stMap: UIO[SMap[EntryKey, StatEntry]] = stChunk_op.map(chnk => chnk.toMap)
		stMap
	}
	def genRandEntryKey(zrnd : ZRandom, keyLen : Int) : UIO[EntryKey] = {
		val (minKeyChr, maxKeyChr) = ('A', 'Z')
		val oneChrOp: UIO[Char] = zrnd.nextIntBetween(minKeyChr, maxKeyChr + 1).map(_.toChar)
		val manyChrStrm = ZStream.repeatZIO(oneChrOp)
		val enoughChrStrm = manyChrStrm.take(keyLen)
		// val acs = ZSink.collectAll[Char]
		val echk: UIO[Chunk[Char]] = enoughChrStrm.runCollect
		val esop: UIO[String] = echk.map(chnk => new String(chnk.toArray))
		esop
	}
	def genManyEKeys (zrnd : ZRandom, keyLen : Int, numKeys : Int) : UIO[Seq[EntryKey]] = {
		val randEkeyOp = genRandEntryKey(zrnd, keyLen)
		val keyStrm = ZStream.repeatZIO(randEkeyOp)
		val enoughKeysOp = keyStrm.take(numKeys).runCollect.map(_.toSeq)
		enoughKeysOp
	}
	// Generate a stream of random stat maps, and treat each as the meat-map of a bin.
	def genMeatInfoStrmFromFixedKeys(zrnd : ZRandom, mathCtx : MathContext, fixedKeys : Seq[EntryKey], fixedFlavor : String, meatGenPrms: MeatGenParams): UStream[BinMeatInfo] = {
		val oneStMpOp = genRandStatMap(zrnd, mathCtx)(fixedKeys, meatGenPrms)
		val stMpStrm = ZStream.repeatZIO(oneStMpOp)
		val miStrm = stMpStrm.map(stMap => BinMeatInfo(fixedFlavor, stMap))
		miStrm
	}

	def mkMassGenOp(zrnd: ZRandom, mathCtx: MathContext)(massGenPrms : MassGenParams): UIO[BigDecimal] = {
		truncGaussBD(zrnd, mathCtx)(massGenPrms.massMin, massGenPrms.massMax)
	}
	def addMassToMeatStrm(meatStrm : UStream[BinMeatInfo], massGenOp : UIO[BigDecimal]) : UStream[(BinMassInfo, BinMeatInfo)] = {
		meatStrm.mapZIO(bmi => massGenOp.map(massBD => (BinMassInfo(massBD, None, None),bmi)))
	}

	// The configuration of this stream is sprinkled in the vals, currently Stu counts 8 of em
	// Each stream tuple is ready to be the genesis of a leaf-bin (with no kiddos)
	def mkMassyMeatStrm(zrnd: ZRandom, mathCtx: MathContext)(binFlav : BinFlavor, meatGenPrms: MeatGenParams, massGenPrms: MassGenParams ) : UStream[(BinMassInfo, BinMeatInfo)] = {

		val ekeysOp : UIO[Seq[BinTypes.EntryKey]] = genManyEKeys(zrnd, meatGenPrms.myKeyLen, meatGenPrms.myNumKeys)
		val ekeyStrmOfSeq = ZStream.fromZIO(ekeysOp)
		val massyMeatStrm : UStream[(BinMassInfo, BinMeatInfo)] = ekeyStrmOfSeq.flatMap(keySeq => {
			val meatInfoStrm = genMeatInfoStrmFromFixedKeys(zrnd, mathCtx, keySeq, binFlav, meatGenPrms)
			val massOp = mkMassGenOp(zrnd, mathCtx)(massGenPrms)
			val massyMeatStrm = addMassToMeatStrm(meatInfoStrm, massOp)
			massyMeatStrm
		})
		massyMeatStrm
	}

}