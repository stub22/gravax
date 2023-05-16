package fun.gravax.distrib.binstore

import fun.gravax.distrib.gen.KnowsBinTupTupTypes
import fun.gravax.distrib.struct.{BinFullKeyInfo, BinMeatInfo}
import zio.cache.{Cache, Lookup}
import zio.{Duration, IO, RIO, URIO, ZIO}
import zio.dynamodb.{ DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}

import java.util.concurrent.TimeUnit

private trait BinCacheStuff

// TODO:  Follow the laws!
// https://zio.dev/reference/service-pattern/the-three-laws-of-zio-environment

trait MeatCacheMaker extends KnowsBinTupTupTypes {
	// We start by fetching just one bin-meat at a time.  For efficiency we might later want to cache+fetch blocks of
	// meat for all children of a parent bin, or even larger blocks.

	protected def getBinWalker : BinWalker
	lazy private val myBinWalker = getBinWalker
	// [BinFullKeyInfo, ZDynDBExec, Throwable, Option[Item]]

	lazy private val myFBI = myBinWalker.getFBI

	def makeMeatyItemCacheOp: URIO[ZDynDBExec, MeatyItemCache] = {
		val timeToLive = Duration.apply(2, TimeUnit.HOURS)
		val capacity = 1000
		val lookupOperation = Lookup(lookupOp)
		// This produces a ZIO op that can produce caches.  We only get the actual cache instance when this op runs,
		// so this value needs to be wired up as a service input.
		Cache.make(capacity, timeToLive, lookupOperation)
	}

	private def lookupOp(binKey : BinFullKeyInfo): RIO[ZDynDBExec, Option[BinMeatInfo]] = {
		val dynPK = binKey.getDynamoPK
		val dynTblNm = binKey.tblNm
		val dynQry = myBinWalker.fetchOneMeatyBinItemAtPK(dynTblNm, dynPK)
		val qryExecOp = dynQry.execute
		val meatFetchOp = qryExecOp.map(opt_itm => opt_itm.map(itm => myFBI.extractMeat(itm)))
		meatFetchOp
	}

	def fetchMeatOp(miCache : MeatyItemCache, binKey : BinFullKeyInfo): IO[Throwable, Option[BinMeatInfo]] = {
		// Hmm we don't want to make the same meat op over and over
		// myMeatCache.flatMap(cget(binKey)
		miCache.get(binKey)
	}



}