package fun.gravax.zaxlam.fblob

import zio.connect.s3.singleregion.SingleRegionS3Connector
import zio.stream.{UStream, ZStream}
import zio.{Chunk, Task, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.util.Date

private trait BlobWriteStuff

object RunDistribBlobSubmitter extends ZIOAppDefault {

	override def run: ZIO[ZIOAppArgs, Any, Any] = {
		println("RunDistribBlobSubmitter.run.BEGIN")
		val x = Console.println("Console.println hooray")
		val textScen = new BlobTextContentScenario {}
		val wiredJob = textScen.mkWiredJob
		println("RunDistribBlobSubmitter.run.END")
		wiredJob
	}
}
	// Write 1:
	// PT1.4764396S,
	// Write 3  (PT1.2615884S,()) (PT0.1190645S,()) (PT0.102239S,())
	/* zio.repeatN : Returns a new effect that repeats this effect the specified number of times or until the first failure.
	Repeats are in addition to the first execution, so that io.repeatN(1) yields an effect that executes io, and then
	if that succeeds, executes io an additional time.
	*/
trait BlobTextContentScenario {
	def mkWiredJob: ZIO[ZIOAppArgs, Any, Any] = {
		val blobber = new StoreDistribBlobVariations {}
		val buckNm = "bux-distrib-ingest-bucket-01"

		val contentPairJob = mkContentPairJob
		val cwriteJob = contentPairJob.flatMap(cpair => {
			val (blobNm, blobStrm) = cpair
			val blobWriteJob = blobber.writeStreamToS3Obj(buckNm, blobNm, blobStrm).map(_ => blobNm)
			blobWriteJob.timed.debug(s"BlobTextContentScenario.mkWiredJob.cwriteJob: blobNm=[${blobNm}]")
		})
		// If we want all the results we need a Stream or...
		val cwriteX3: ZIO[SingleRegionS3Connector, Object, (zio.Duration, String)] = cwriteJob.repeatN(2)
		val wiredJob = blobber.wireAwsJobSingR(cwriteX3)
		wiredJob

	}
	val mkContentPairJob: Task[(String, UStream[Byte])] = {
		ZIO.attempt {
			val blobStrm = mkFreshBlobStrm
			val blobNm = "dumTxtBlob_" + System.currentTimeMillis() + ".txt"
			(blobNm, blobStrm)
		}
	}
	def mkFreshBlobStrm : UStream[Byte] = {
		val tqs =
			"""This is a triple-quoted String defined in RunDistribBlobSubmitter.
			  |Second line is here.
			  |Third and final line.
			  |""".stripMargin
		val dateTxt = new Date().toString
		val fullTxt = tqs + "\n" + dateTxt + "\n=========================================\n"
		val blobBytes: Array[Byte] = fullTxt.getBytes
		val blobChunk = Chunk.fromArray(blobBytes)
		val blobStrm = ZStream.fromChunk(blobChunk)
		blobStrm
	}
}
trait StoreDistribBlobVariations {
	import software.amazon.awssdk.regions.Region
	import zio._
	import zio.aws.core.AwsError
	import zio.aws.core.config.AwsConfig
	import zio.aws.netty.NettyHttpClient
	import zio.aws.s3.S3
	import zio.aws.s3.model.primitives.{BucketName, ObjectKey}
	import zio.connect.s3._
	// #121  " S3 connector split into a single and multiregion version(
	import zio.connect.s3.multiregion._
	import zio.stream._

	lazy val zioAwsConfig = NettyHttpClient.default >>> AwsConfig.default
	// lazy val region = Region.US_WEST_2

	val x: ZIO[Any, Nothing, Array[Byte]] = Random.nextString(100).map(_.getBytes)
	val xchnk: ZIO[Any, Nothing, Chunk[Byte]] = x.map(Chunk.fromArray)
	lazy val xcxc : Chunk[Byte] = ???
	lazy val xstrm: ZStream[Any, Nothing, Byte] = ZStream.fromChunk(xcxc)
	lazy val pxstrm : UStream[Byte] = xstrm

	def writeStreamToS3Obj(bucketNameTxt : String, objKeyTxt : String, strm : UStream[Byte]) = {

		val bucketName = BucketName(bucketNameTxt)
		val objectKey = ObjectKey(objKeyTxt)
		val singOpSink: ZSink[SingleRegionS3Connector, AwsError, Byte, Nothing, Unit] = putObject(bucketName, objectKey)
		val singOpEff: ZIO[SingleRegionS3Connector, AwsError, Unit] = strm >>> singOpSink
		// val multiOpSink: ZSink[MultiRegionS3Connector, AwsError, Byte, Nothing, Unit] = putObject(bucketName, objectKey, region)
		// val multiOpEff: ZIO[MultiRegionS3Connector, AwsError, Unit] = strm >>> multiOpSink
		singOpEff
	}

	// s3ConnectorLiveLayer
	// multiRegionS3ConnectorLiveLayer
	def wireAwsJobSingR[Output](prog : ZIO[SingleRegionS3Connector, Object, Output]) = {
// 	def wireAwsJobSingR[Output](prog : ZIO[SingleRegionS3Connector, AwsError, Output]) = { // : Task[Unit] = {
		val wiredJob = prog.provide(zioAwsConfig, S3.live, s3ConnectorLiveLayer)
				.tapBoth(
					error => Console.printLine(s"wireAwsJob.tapBoth.error: ${error}"),
					data => Console.printLine(s"wireAwsJob.tapBoth.data: ${data}"))
		wiredJob
	}
	/*
	def wireAwsJobMultiR(prog : ZIO[MultiRegionS3Connector, AwsError, Unit]) = { //  : Task[Unit] = {
// For MultiR We need to provide a map of regions, which is not shown in the S3 connector examples we are starting from

Please provide a layer for the following type:
Required by `package`.multiRegionS3ConnectorLiveLayer
1. scala.collection.immutable.Map[software.amazon.awssdk.regions.Region,zio.aws.s3.S3]

		val wiredJob = prog.provide(zioAwsConfig, S3.live, multiRegionS3ConnectorLiveLayer)
				.tapBoth(
					error => Console.printLine(s"wireAwsJob.tapBoth.error: ${error}"),
					data => Console.printLine(s"wireAwsJob.tapBoth.data: ${data}"))
		wiredJob
	}
	*/
}

trait ZioS3ConnEx2 {
	/*
	Example code copied from:
	https://github.com/zio/zio-connect/blob/master/examples/s3-connector-examples/src/main/scala/Example2.scala
	 */

	import software.amazon.awssdk.regions.Region
	import zio._
	import zio.aws.core.AwsError
	import zio.aws.core.config.AwsConfig
	import zio.aws.netty.NettyHttpClient
	import zio.aws.s3.S3
	import zio.aws.s3.model.primitives.{BucketName, ObjectKey}
	import zio.connect.s3._
	import zio.connect.s3.multiregion._
	import zio.stream._

	lazy val zioAwsConfig = NettyHttpClient.default >>> AwsConfig.default
	lazy val region = Region.US_EAST_1

	val bucketName = BucketName("this-very-charming-bucket-name") // BucketName is a zio prelude newtype of String

	val program1: ZIO[MultiRegionS3Connector, AwsError, Unit] =
		for {
			_ <- ZStream(bucketName) >>> createBucket(region)
		} yield ()

	val objectKey = ObjectKey("my-object") // ObjectKey is a zio prelude newtype of String

	val program2: ZIO[MultiRegionS3Connector, AwsError, Unit] =
		for {
			content <- Random.nextString(100).map(_.getBytes).map(Chunk.fromArray)
			_ <- ZStream.fromChunk(content) >>> putObject(bucketName, objectKey, region)
		} yield ()

	val program3: ZIO[MultiRegionS3Connector, AwsError, Chunk[ObjectKey]] =
		for {
			keys <- listObjects(bucketName, region).runCollect
		} yield keys

	val program4: ZIO[MultiRegionS3Connector, Object, String] =
		for {
			content <- getObject(bucketName, objectKey, region) >>> ZPipeline.utf8Decode >>> ZSink.mkString
		} yield content

	val program5: ZIO[MultiRegionS3Connector, AwsError, Unit] =
		for {
			_ <- ZStream(objectKey) >>> deleteObjects(bucketName, region)
			_ <- ZStream(bucketName) >>> deleteEmptyBucket(region)
		} yield ()
}

