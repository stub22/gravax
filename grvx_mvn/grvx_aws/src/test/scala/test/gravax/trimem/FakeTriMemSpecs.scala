package test.gravax.trimem

import org.scalatest.flatspec.AnyFlatSpec
import org.slf4j.LoggerFactory

private trait FakeTriMemSpecs

class FirstFakeTriMemSpec() extends AnyFlatSpec {
	val myTMO = new TriMemOps {}
	"A TriMem" should "do good stuff" in {
		myTMO.go
	}

}
trait TriMemOps {
	def go : Unit = {
		println("TriMemOps.go println HELLO")
		loggn
		println("TriMemOps.go println GOOBEY")  // like GOODBYE but better
	}

	def loggn : Unit = {
		val ourSlf4JLog = LoggerFactory.getLogger(this.getClass)
		// As of 22-02-12 we do see this warn+info output to stdout when running test-specs from
		// IDEA 2020.2 (jbrains), Scala 2.13, JDK 8 (oracle) on Windows 10 (msoft).
		ourSlf4JLog.warn(s"Log.Warn sent to slf4jLog: ${ourSlf4JLog}")
		ourSlf4JLog.info(s"Log.Info YES we have slf4j dep via our axmgc dependency.")
		// As of 22-02-12 do not expect to see this with latest snaps, because log4j.appender.stdout.Threshold=INFO
		ourSlf4JLog.debug(s"Log.Debug WOW backend unfixed pariah log4j config coming in from axmgc.web.pond rscrc log4j.properties")
	}
}