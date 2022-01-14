package fun.gravax.gravtst.console

import java.io.StringReader

import axmgc.dmo.fin.ontdmp.OntQryMgr
import fun.gravax.gravcld.ReadS3Files
import org.slf4j.{Logger, LoggerFactory}

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
	private val myS4JLog = LoggerFactory.getLogger(this.getClass)
	def doSomethin : Unit = {
		myS4JLog.info("GravCloudPing.doSomethin says HEYO")
		println("GravCloudPing.doSomethin using regular scala println to say HEY")
		val s3rd = new ReadS3Files {
			override protected def analyzeN3DataBlob(n3blob: String, modelPath : String): Unit = {
				val lyzer = new N3Analyzer{}
				val mdl = lyzer.loadModelFromText(n3blob, modelPath)
				lyzer.chkModelStats(modelPath, mdl)
			}
		}
		s3rd.doBucketStuff
	}

}

import org.apache.jena.rdf.model.{ModelFactory, Model => JenaMdl}
import org.apache.jena.riot.{Lang, RDFDataMgr}
trait N3Analyzer {
	private val myS4JLog : Logger = LoggerFactory.getLogger(this.getClass)
	def loadModelFromText(blob : String, mdlRsrcPath : String) : JenaMdl = {
		val txtRdr = new StringReader(blob)
		val jmdl = ModelFactory.createDefaultModel()
		RDFDataMgr.read(jmdl, txtRdr, mdlRsrcPath, Lang.N3)
		myS4JLog.info("Finished load from {}, final model size is: {}", mdlRsrcPath, jmdl.size())
		jmdl
	}
	private val myOQM = new OntQryMgr{}
	def chkModelStats(modelName : String, jenaMdl : JenaMdl) : Unit = {
		val statJsnTxt = myOQM.dumpMdlStatsToJsnArrTxt(jenaMdl)
		myS4JLog.info(s"Collected stats for modelName=${modelName}")
		myS4JLog.info("Stats as JsonArray: {}", statJsnTxt)
	}

}
