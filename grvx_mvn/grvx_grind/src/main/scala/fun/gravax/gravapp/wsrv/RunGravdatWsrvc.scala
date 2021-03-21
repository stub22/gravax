// Copyright 2021 Stu Btwotwo.  Distributed under Apache License 2.0.  See bottom of this file.

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

/***
--------------------------------------------------------------
Copyright 2021 Stu Btwotwo
See GRAVAX_LICENSE_APACHE2.txt and GRAVAX_CREDITS.txt
--------------------------------------------------------------
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 ***/