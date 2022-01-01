package fun.gravax.gravcld

import java.time.Instant
import java.util.{List => JList}
import java.util.stream

import software.amazon.awssdk.core.SdkField
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{Bucket, ListBucketsRequest, ListBucketsResponse, ListObjectsRequest, ListObjectsResponse, S3Object}

import scala.collection.immutable

private trait S3ReadStuff

/*
https://github.com/awsdocs/aws-doc-sdk-examples/tree/main/javav2/example_code/s3/src/main/java/com/example/s3


 */
trait ReadS3Files {
	val bcktNm_appOntMiscPub = "appstract-ont-misc-pub"

	def doBucketStuff : Unit = {
		val cli = mkS3Client
		listSomeBuckets(cli)
	}
	def mkS3Client : S3Client = {
		val region = Region.US_WEST_2;
		println("Building s3Cli")
		/*
	https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/S3ClientBuilder.html
	BuilderT region(Region region)
	Configure the region with which the SDK should communicate.
	If this is not specified, the SDK will attempt to identify the endpoint automatically using the following logic:

	Check the 'aws.region' system property for the region.
	Check the 'AWS_REGION' environment variable for the region.
	Check the {user.home}/.aws/credentials and {user.home}/.aws/config files for the region.
	If running in EC2, check the EC2 metadata service for the region.
	If the region is not found in any of the locations above, an exception will be thrown at SdkBuilder.build() time.
		 */

		val s3cli = S3Client.builder().region(region).build();
		/*
https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/S3Client.html
		 */
		println(s"Made s3Cli ${s3cli.toString}")
		s3cli
	}
	def listSomeBuckets(s3cli : S3Client) : Unit = {
		println("Building buckets RQ")
		val listBucketsRequest = ListBucketsRequest.builder().build();
		println("Calling listBuckets")
		/*
default ListBucketsResponse	listBuckets()
Returns a list of all buckets owned by the authenticated sender of the request.
https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/model/ListBucketsRequest.html
https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/model/ListBucketsResponse.html
		 */
		val listBucketsResponse: ListBucketsResponse = s3cli.listBuckets(listBucketsRequest);
		val bcktNmLst: List[String] = printBucketResponse(listBucketsResponse)
		bcktNmLst.foreach(bcktNm => {
			listBucktObjKeys(s3cli, bcktNm)
		})
		// val bstrm: stream.Stream[Bucket] = listBucketsResponse.buckets().stream()
		// forEach(x -> System.out.println(x.name()));
	}
	import scala.jdk.CollectionConverters._

	def printBucketResponse(lbucktsResp: ListBucketsResponse) : List[String] = {

		val bJList: JList[Bucket] = lbucktsResp.buckets()
		val bSList: List[Bucket] = bJList.asScala.toList
		bSList.foreach(bckt => {
			/*
			https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/model/Bucket.html
			 */
			println(s"Found bckt: ${bckt.toString}")
			val bn: String = bckt.name()
			val cd: Instant = bckt.creationDate()
			val sfl = bckt.sdkFields()
			println(s"SdkFields: ${sfl.toString}")
		})

		val bcktNms = bSList.map(_.name())
		bcktNms
	}

	// What if we want to use s3URI ?
	def listBucktObjKeys(s3cli : S3Client, bcktNm : String) : List[String] = {
		val lorq = 	ListObjectsRequest.builder().bucket(bcktNm).build();

		val loResp: ListObjectsResponse = s3cli.listObjects(lorq)
		val objJLst: JList[S3Object] = loResp.contents()

		val objSList : List[S3Object] = objJLst.asScala.toList
		objSList.foreach(ob => {
			val obk: String = ob.key()
			println(s"Found obj: ${ob.toString}")
		})
		val bcktKys = objSList.map(_.key)
		bcktKys
	}
	/*
            ListObjectsRequest listObjects = ListObjectsRequest
                    .builder()
                    .bucket(bucketName)
                    .build();

            ListObjectsResponse res = s3.listObjects(listObjects);
            List<S3Object> objects = res.contents();

            for (S3Object myValue: objects) {
                keyName = myValue.key();
                keys.add(keyName);
            }
            return keys;
	 */
}
