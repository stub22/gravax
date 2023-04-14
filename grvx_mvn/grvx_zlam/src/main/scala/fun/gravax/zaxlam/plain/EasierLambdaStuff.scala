package fun.gravax.zaxlam.plain

import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger, RequestHandler}
import java.util.{Map => JMap}
private trait EasierLambdaStuff


trait HandlerMon {
	def dumpCtx(lamLog: LambdaLogger, ctx: Context) : Unit = {
		lamLog.log(s"HandlerMon.dumpCtx got ctx: ${ctx}")
	}
	def dumpEnv(lamLog: LambdaLogger) : Unit = {
		val envJMap: JMap[String, String] = System.getenv()
		lamLog.log(s"HandlerMon.dumpEnv got envJMap: ${envJMap}")
	}
}
trait AxLamHandler[In,Out] extends RequestHandler[In,Out] {
	val myMon = new HandlerMon{}

	protected def handleRq(in : In, ctx: Context): Out
	override def handleRequest(in : In, context: Context): Out = {
		val lamLog: LambdaLogger = context.getLogger
		myMon.dumpEnv(lamLog)
		println("HelloZaxlam.handleRequest is printing to console?!")
		myMon.dumpCtx(lamLog, context)
		lamLog.log(s"AxLamHandler.handleRequest is naively dumping input: ${in}")
		val out = handleRq(in, context)
		lamLog.log(s"AxLamHandler.handleRequest is naively dumping output: ${out}")
		out
	}

}
class HelloZaxlam() extends AxLamHandler[JMap[String,String], String] {
	override protected def handleRq(in: JMap[String, String], ctx: Context): String = "HELLO_ZAXLAM_RESULT_v007"
}

class HowdyZaxlam() extends AxLamHandler[String, String] {
	override protected def handleRq(in: String, ctx: Context): String = "HowdyZaxlam_result_v007"
}