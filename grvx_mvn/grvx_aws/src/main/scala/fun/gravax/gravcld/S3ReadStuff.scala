package fun.gravax.gravcld

import java.time.Instant
import java.util.{List => JList}
import java.util.stream

import software.amazon.awssdk.core.{ResponseBytes, SdkField}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{Bucket, GetObjectRequest, GetObjectResponse, ListBucketsRequest, ListBucketsResponse, ListObjectsRequest, ListObjectsResponse, S3Object}

import scala.collection.immutable

private trait S3ReadStuff

/*
S3 example code for API version 2
https://github.com/awsdocs/aws-doc-sdk-examples/tree/main/javav2/example_code/s3/src/main/java/com/example/s3

S3 client API
https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/S3Client.html

SDK Src
https://github.com/aws/aws-sdk-java-v2
 */
trait ReadS3Files {
	val bcktNm_appOntMiscPub = "appstract-ont-misc-pub"

	val objKey_kbPrefixes = "kbpedia_v250/supp/kbprc_prefixes.n3"
	val (flg_scanBuckets, flg_loadText, flg_loadN3) = (false, false, true)
	def doBucketStuff : Unit = {
		val cli = mkS3Client
		if (flg_scanBuckets)
			listSomeBuckets(cli)
		if (flg_loadText) {
			fetchSomeObjsInBucket(cli, bcktNm_appOntMiscPub, 5, 1000, 50000)
		}
		if (flg_loadN3) {
			val kbPrfxTxt = loadKbPrefixesText(cli, bcktNm_appOntMiscPub, objKey_kbPrefixes)
			loadSomeSmallN3files(cli, bcktNm_appOntMiscPub, Some(kbPrfxTxt))
		}
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
			println(s"Calling listBucktObjRefs for bcktNm=${bcktNm}")
			val objRefs = listBucktObjRefs(s3cli, bcktNm)
			printObjRefInfo(objRefs)
		})
		// val bstrm: stream.Stream[Bucket] = listBucketsResponse.buckets().stream()
		// forEach(x -> System.out.println(x.name()));
	}

	def fetchSomeObjsInBucket(s3cli : S3Client, bcktNm : String, maxObjs : Int, minObjSize : Long, maxObjSize : Long) : Unit = {
		val objRefs = listBucktObjRefs(s3cli, bcktNm)
		val smallEnuffObjs = objRefs.filter(s3obj => ((s3obj.size() <= maxObjSize) && (s3obj.size >= minObjSize)))
		val firstSmallObjs = smallEnuffObjs.take(maxObjs)
		println(s"In bucket ${bcktNm} found ${objRefs.length} total objects.")
		println(s"${smallEnuffObjs.length} objects have ${minObjSize} <= size <= ${maxObjSize}.  Selected ${firstSmallObjs.length} of these.")
		firstSmallObjs.foreach(s3obj => {
			val objKy = s3obj.key()
			val objAsUtf8 = getBucktObjBytesAsUtf8(s3cli, bcktNm, objKy)
			println(s"Got text of length ${objAsUtf8.length} from objKey: ${objKy} in bucket: ${bcktNm}")
			val previewTxt = objAsUtf8.take(96)
			println(s"First 96 chars: " , previewTxt)
		})
	}
	def findObjsInBucket(s3cli : S3Client, bcktNm : String, nmSuffix : String, minObjSize : Long, maxObjSize : Long) :
			List[S3Object] = {
		val objRefs = listBucktObjRefs(s3cli, bcktNm)
		println(s"In bucket ${bcktNm} found ${objRefs.length} total objects.")
		val matchingObjs = objRefs.filter(s3obj => {
			val sz = s3obj.size
			val nm = s3obj.key()
			val matchHit = nm.endsWith(nmSuffix) && (sz >= minObjSize) && (sz <= maxObjSize)
			if (matchHit) println(s"Matched object with name=${nm} and size=${sz}")
			matchHit
		})
		println(s"Found ${matchingObjs.length} matching objects within size range ")
		matchingObjs
	}
	protected def analyzeN3DataBlob(n3blob : String, modelPath : String)
	def loadSomeSmallN3files(s3cli : S3Client, bcktNm : String, prefixTxtBlk : Option[String]) : Unit = {
		val maxObjs = 5
		val goodObjs = findObjsInBucket(s3cli, bcktNm, ".n3", 1000, 80000)
		val firstObjs = goodObjs.take(maxObjs)
		println(s"Selected ${firstObjs.length} objects expected to contain N3 data, which we now try to load.")
		println(s"Objs to load: ", firstObjs.toString())
		firstObjs.foreach(s3obj => {
			val objKy: String = s3obj.key()
			val objAsUtf8: String = getBucktObjBytesAsUtf8(s3cli, bcktNm, objKy)
			println(s"Got N3 text of length ${objAsUtf8.length} from objKey: ${objKy} in bucket: ${bcktNm}")
			val previewTxt = objAsUtf8.take(99)
			println(s"First 99 chars: " , previewTxt)
			val comboTxt = prefixTxtBlk.getOrElse("").concat(objAsUtf8)
			println(s"After prepending prefix, combo txt length is ${comboTxt.length}")
			analyzeN3DataBlob(comboTxt, objKy)
		})
	}
	def loadKbPrefixesText(s3cli : S3Client, bcktNm : String, kbprfxObjKey : String) : String = {
		val kbprfxTxt = getBucktObjBytesAsUtf8(s3cli, bcktNm, kbprfxObjKey)
		println(s"Loaded kb-prefix text of length=${kbprfxTxt.length}")
		println(s"First 127 chars: ", kbprfxTxt.take(127))
		kbprfxTxt
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
	def listBucktObjRefs(s3cli : S3Client, bcktNm : String) : List[S3Object]  = {
		val lorq = 	ListObjectsRequest.builder().bucket(bcktNm).build();

		val loResp: ListObjectsResponse = s3cli.listObjects(lorq)
		val objJLst: JList[S3Object] = loResp.contents()

		val objSList : List[S3Object] = objJLst.asScala.toList

		objSList
	}

	def printObjRefInfo(objSList : List[S3Object]) : Unit = {
		objSList.foreach(ob => {
			val obk: String = ob.key()
			val obSize = ob.size()
			println(s"Found obj: ${ob.toString}")
		})
	}
	def getBucktObjBytesAsUtf8(s3cli : S3Client, bcktNm : String, objKy : String) : String = {
		val gobjRq = buildGetObjRq(bcktNm, objKy)
		val respBytesObj: ResponseBytes[GetObjectResponse] = s3cli.getObjectAsBytes(gobjRq)
		respBytesObj.asUtf8String()
		//  GetObjectData.getObjectBytes(s3,bucketName,objectKey, path);
	}
	def buildGetObjRq(bcktNm : String, objKy : String) : GetObjectRequest = {
		val emptyBldr = GetObjectRequest.builder()
		val readyBldr = emptyBldr.bucket(bcktNm).key(objKy)
		val rq = readyBldr.build()
		rq
	}
	def UNUSED_getBucktObjContentAsUtf8(s3obj : S3Object) : Option[String] = {
		val owner = s3obj.owner()
		val key = s3obj.key()
		None
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


/*
https://github.com/awsdocs/aws-doc-sdk-examples/blob/main/javav2/example_code/s3/src/main/java/com/example/s3/GetObjectData.java

            GetObjectRequest objectRequest = GetObjectRequest
                    .builder()
                    .key(keyName)
                    .bucket(bucketName)
                    .build();

            ResponseBytes<GetObjectResponse> objectBytes = s3.getObjectAsBytes(objectRequest);
            byte[] data = objectBytes.asByteArray();

 */