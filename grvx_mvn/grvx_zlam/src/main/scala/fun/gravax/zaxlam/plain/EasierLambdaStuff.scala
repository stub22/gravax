package fun.gravax.zaxlam.plain

import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger, RequestHandler}
import java.util.{Map => JMap}
private trait EasierLambdaStuff


class HelloZaxlam() extends RequestHandler[JMap[String,String], String] {

	override def handleRequest(inMap: JMap[String, String], context: Context): String = {
		val lamLog: LambdaLogger = context.getLogger
		lamLog.log(s"HelloZaxlam.handleRequest got inMap: ${inMap}")
		val envJMap: JMap[String, String] = System.getenv()
		lamLog.log(s"HelloZaxlam.handleRequest got envJMap: ${inMap}")
		println("HelloZaxlam.handleRequest is printing to console")
		"HELLO_ZAXLAM_RESULT_v006"
	}
}
