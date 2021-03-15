package fun.gravax.gravapp.wsrv

import fun.gravax.gravsys.log.LoggingLauncher

object RunGravdatWsrvc {
	def main(args: Array[String]): Unit = {
		goRunSvcLaunch
	}
	def goRunSvcLaunch : Unit = {
		val loggingLauncher = new LoggingLauncher {}
		loggingLauncher.setup
		val bwl = new GravdatWsrvcLaunch {}
		bwl.launchGravdatWsrvc
	}
}

trait GravdatWsrvcLaunch {
	def launchGravdatWsrvc : Unit = launchDataTstWeb

	private lazy val myBWTApp = new GravdatWsvcTstApp()
	private def launchDataTstWeb(): Unit = {
		val srvIntf = "localhost"
		val srvPort = 8333
		val actSysNm = "grvwdact"
		myBWTApp.launchTstWbsrvc(actSysNm, srvIntf, srvPort)
	}
}

