package fun.gravax.gravapp.wsrv

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.{server => dslServer}
import axmgc.web.pond.WebServerLauncher
import axmgc.web.sssn.WbSssnStBossFactory
/* DISABLED IMPORTS
Will probly use most of these features, but so far our skeleton doesn't need them.
import axmgc.web.cors.CORSHandler
import axmgc.web.pond.{RouteWeaver}
import axmgc.web.rsrc.WebRsrcRouteMkr
import spray.json.{DefaultJsonProtocol, JsObject, JsValue}
 */
import org.slf4j.LoggerFactory

private trait GravdatWsrvcImplStuff

private class GravdatRouteCtx {
	val myAppLggr = LoggerFactory.getLogger(this.getClass)
	import dslServer.Directives.{_} // Establishes  ~  and whatnot
	def mkGravdatRootRoute(actrSys: ActorSystem) : dslServer.Route = {
		val firstWepBoss : ActorRef = WbSssnStBossFactory.launchWebEventProc(actrSys, "frst01")
		myAppLggr.info("Launched RouteWeaver event-boss actor: " + firstWepBoss)
		val routeBuilder = new GravTupleRouteBuilder{
			override protected def rmFindHlpActRef(sessID: Long): ActorRef = {
				myAppLggr.info("Returning wep-boss actor:", firstWepBoss)
				firstWepBoss
			}
		}
		routeBuilder.getGravTplRt
	}
}

class GravdatWsvcTstApp() {
	val myAppLggr = LoggerFactory.getLogger(this.getClass)

	private lazy val myWsvcLnchr = new WebServerLauncher {}

	def launchTstWbsrvc(actSysNm: String, hostNm: String, portNum: Int): Unit = {
		val actSys: ActorSystem = myWsvcLnchr.makeActorSys(actSysNm)
		val wbSvcRt = mkWbSvcRt(actSys)
		myWsvcLnchr.launchWebServer(wbSvcRt, actSys, hostNm, portNum)
	}

	private val myRouteStuff = new GravdatRouteCtx {}
	protected def mkWbSvcRt(actrSys: ActorSystem): dslServer.Route = myRouteStuff.mkGravdatRootRoute(actrSys)
}

