package fun.gravax.gravtst.console

import java.io.File
import java.lang
import java.net.{URI, URL}
import java.nio.file.{DirectoryStream, FileSystem, FileSystems, Files, Path}

import axmgc.dmo.ksrc.lean_mthlb.LmlNavItemMaker
import fun.gravax.gravsys.log.LoggingLauncher
import org.slf4j.LoggerFactory

object RunGravchkConsole {
	def main(args: Array[String]): Unit = {
		goDoStuff
	}
	def goDoStuff : Unit = {
		val loggingLauncher = new LoggingLauncher {}
		loggingLauncher.setup
		val gct = new GravchkConsoleTests
		// Point here is to access classpath resources from a maven jar (in an outside dependency), although we
		// may be running in a incrementally compiled .idea context
		gct.chkPondRsrcs
	}
}
class GravchkConsoleTests {
	private val myS4JLog = LoggerFactory.getLogger(this.getClass)
	def chkLmlNavStuff : Unit = {
//		val lnim = new LmlNavItemMaker
//		val bigTree = lnim.mkBigTree()
//		myS4JLog.info("Got bigTree: {}", bigTree)
		chkPondRsrcs
	}

	def chkPondRsrcs : Unit = {
		val pondClz = classOf[axmgc.web.json.Person]
		chkFolderNaive(pondClz, "/wdat/")
	}
	def chkFolderNaive(mrkClz : Class[_], fpath : String) : Unit = {
		val fldrUrl: URL = mrkClz.getResource(fpath)
		myS4JLog.info("checking folderURL={}", fldrUrl)
		checkFolderUrl(fldrUrl)
	}
	def checkFolderUrl (fldrUrl : URL) : Unit = {
		val fldrUri: URI = fldrUrl.toURI
		myS4JLog.info("folderUri={}", fldrUri)
		if (true) {
			chkFileSys(fldrUri)
		} else {
			val dirf = new File(fldrUri)
			if (dirf.isDirectory) {
				val dlst: Array[File] = dirf.listFiles()
				dlst.foreach(f => {
					val fobj: File = f
					myS4JLog.info("Found file={}", f)
				})
			}
		}
	}
	import scala.collection.JavaConverters._
	def chkFileSys(fsUri : URI) : Unit = {
		val fsParamsScala = Map[String, AnyRef]()
		val fsParamsJava = fsParamsScala.asJava
		// val fsParams = new java.util.HashMap[String, _]()
		val fs: FileSystem =  FileSystems.newFileSystem(fsUri, fsParamsJava)
		fs.close()
	}
	private def chkRootPaths(fs: FileSystem) : Unit = {
		val rootDirs: lang.Iterable[Path] = fs.getRootDirectories
		val rdSc: List[Path] = rootDirs.asScala.toList
		myS4JLog.info("Got root paths: {}", rdSc)
		rdSc.foreach(rootPath => {
			myS4JLog.info("Inspecting rootDir={}", rootPath)
			chkNioDirPath(rootPath)
		})
	}
	def chkNioDirPath(npDirPath : Path) : Unit = {
		myS4JLog.info("Checking dirPath={}, which is part of fileSys={}", npDirPath, npDirPath.getFileSystem : Any)
		val dirStrm: DirectoryStream[Path] = Files.newDirectoryStream(npDirPath)
		val cnts: List[Path] = dirStrm.asScala.toList
		dirStrm.close()
		myS4JLog.info("In path={}, found  dir-contents: {}", npDirPath,  cnts : Any)

	}

}
