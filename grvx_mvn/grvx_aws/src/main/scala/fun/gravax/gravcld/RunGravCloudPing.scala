package fun.gravax.gravtst.console

import fun.gravax.gravcld.ReadS3Files

/*
import java.io.File
import java.lang
import java.net.{URI, URL}
import java.nio.file.{DirectoryStream, FileSystem, FileSystems, Files, Path}

import axmgc.dmo.ksrc.lean_mthlb.LmlNavItemMaker
import fun.gravax.gravsys.log.LoggingLauncher

 */
// import org.slf4j.LoggerFactory

object RunGravCloudPing {
	def main(args: Array[String]): Unit = {
		goDoStuff
	}
	def goDoStuff : Unit = {
		// val loggingLauncher = new LoggingLauncher {}
		// loggingLauncher.setup
		val gcp = new GravCloudPing
		// Access some cloud resources
		gcp.doSomethin
	}
}
class GravCloudPing {
	private val myS4JLog = 0 /// ??? //  LoggerFactory.getLogger(this.getClass)
	def doSomethin : Unit = {
		// myS4JLog.info("GravCloudPing.doSomethin says HEYO")
		println("GravCloudPing.doSomethin using regular scala println to say HEY")
		val s3rd = new ReadS3Files {}
		s3rd.doBucketStuff
	}

}
