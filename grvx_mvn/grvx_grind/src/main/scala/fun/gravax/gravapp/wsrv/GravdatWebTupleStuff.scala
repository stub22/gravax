package fun.gravax.gravapp.wsrv

import akka.http.scaladsl.{server => dslServer}
import axmgc.web.ent.{HtEntMkr, WebXmlGen}
import axmgc.web.lnkdt.LDChunkerTest
import axmgc.web.pond.RouteWeaver
import axmgc.web.tuple.{IntrnlPonderRslt, WTRouteMaker, WebRqPrms, WebTupleMaker}
import org.slf4j.{Logger, LoggerFactory}

private trait GravdatWebTupleStuff

trait GravTupleRouteBuilder extends RouteWeaver {
	protected val ourSlf4JLog = LoggerFactory.getLogger(this.getClass)
	private val gravTplTst = "grvtpl"
	protected lazy val myGravTplMkr = new WebTupleMaker {
		override protected def getLDChnkr: LDChunkerTest = myLDChnkr
		override protected def getHtEntMkr: HtEntMkr = myHtEntMkr
		override protected def getWebXml: WebXmlGen = myXEntMkr

		override protected def doPageWork(rqPrms: WebRqPrms): Option[IntrnlPonderRslt] = {
			ourSlf4JLog.info(s"Doing page work in GravTupleRouteBuilder.myGravTplMkr for prms=${rqPrms}")
			None
		}
		override def evalFullPageNow(pgEvalCtx : PgEvalCtx, chainBk: Boolean) : PgEntTpl = {
			ourSlf4JLog.info(s"Doing evalFullPageNow in GravTupleRouteBuilder.myGravTplMkr for ctx=${pgEvalCtx}, chainBk=${chainBk}")
			val supTpl = super.evalFullPageNow(pgEvalCtx, chainBk)
			val bttrTpl = supTpl
			ourSlf4JLog.info(s"Completed evalFullPageNow in GravTupleRouteBuilder.myGravTplMkr with bttrTpl = ${bttrTpl}")
			bttrTpl
		}
	}
	protected lazy val myGravTplRtMkr = new WTRouteMaker {
		override protected def getWbTplMkr: WebTupleMaker = myGravTplMkr

		override protected def getPathTxt: String = gravTplTst
	}
	protected lazy val myGravTplRt: dslServer.Route = myGravTplRtMkr.makeWbTplRt(ourSlf4JLog)
	def getGravTplRt : dslServer.Route = myGravTplRt
}
