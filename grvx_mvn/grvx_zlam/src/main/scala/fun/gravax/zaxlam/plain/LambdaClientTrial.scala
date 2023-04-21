package fun.gravax.zaxlam.plain

import org.json.JSONObject
import software.amazon.awssdk.auth.credentials.ProfileCredentialsProvider
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.lambda.LambdaClient
import software.amazon.awssdk.services.lambda.model.InvokeRequest

private trait LambdaClientTrial

/*
Adapted from:
https://github.com/awsdocs/aws-doc-sdk-examples/blob/main/javav2/example_code/lambda/src/main/java/com/example/lambda/LambdaInvoke.java
 */

object RunZaxlamClientTrials {
	def main(args: Array[String]): Unit = {
		val lamInvk = new LambdaInvoker {}
		val howdyArgTxt = "input txt from LambdaInvoker.runHowdy"
		val howdyRsltTxt = lamInvk.runHowdy(howdyArgTxt)
		println(s"Got howdyRslt: ${howdyRsltTxt}")
		val helloRsltTxt = lamInvk.runHello("apple", "banana")
		println(s"Got helloRslt: ${helloRsltTxt}")
	}
}
trait LambdaInvoker {
	val funcPrefix_zaxLamFuncs = "arn:aws:lambda:us-west-2:693649829226:function:"
	val funcNm_Hello : String = funcPrefix_zaxLamFuncs + "HelloZaxlam_007" //  "arn:aws:lambda:us-west-2:693649829226:function:HelloZaxlam_007" // args(0)
	val funcNm_Howdy : String = funcPrefix_zaxLamFuncs + "howdyZaxlam_007" // "arn:aws:lambda:us-west-2:693649829226:function:howdyZaxlam_007"
	val region  = Region.US_WEST_2
	val credProv = ProfileCredentialsProvider.create
	val awsLambda = LambdaClient.builder.region(region).credentialsProvider(credProv).build

	// invokeFunction(awsLambda, functionName)

	def runHowdy(inArgTxt : String) : String = {
		// FIXME:  Have not figured out how to make Lambda infra accept a single String as valid JSON, even though
		// they show some examples working this way.
		// TODO:  Adapt to use more friendly scala JSON library.
		val inJSON: JSONObject = new JSONObject()
		inJSON.put("in", inArgTxt);
		// val otherJSON = new JSONObject("\"" + inArgTxt + "\"")
		runFunc(funcNm_Howdy, inJSON)

	}
	def runHello(inArg01 : String, inArg02 : String) : String = {
		// This works to deliver a java.Map<String,String> after JSON decoding in the service.
		val inJSON: JSONObject = new JSONObject()
		inJSON.put("in01", inArg01)
		inJSON.put("in02", inArg02)
		runFunc(funcNm_Hello, inJSON)
	}

	def runFunc(funcNm : String, inJsonObj : JSONObject): String = {
		val jsonBlobTxt = inJsonObj.toString()
		println(s"LambdaInvoker.runFunc built jsonBlobTxt: ${jsonBlobTxt}")
		val blobBytes: SdkBytes = SdkBytes.fromUtf8String(jsonBlobTxt) ;

		val rqst = InvokeRequest.builder().functionName(funcNm)
				.payload(blobBytes)
				.build();

		val resp = awsLambda.invoke(rqst);
		println(s"resp.toString = ${resp}")
		println(s"resp.statusCode = ${resp.statusCode()}")
		println(s"resp.logResult= ${resp.logResult()}")
		val respBytes: SdkBytes = resp.payload()
		val respTxt = respBytes.asUtf8String()
		respTxt
	}
}