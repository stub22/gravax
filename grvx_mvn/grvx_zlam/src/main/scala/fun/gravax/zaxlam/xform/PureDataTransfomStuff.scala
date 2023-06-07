package fun.gravax.zaxlam.xform

import java.lang.{Boolean => JBool, Double => JDoub, Integer => JInt}
import scala.collection.immutable.{Map => SMap}
import java.util.{List => JList, Map => JMap}
import scala.jdk.CollectionConverters._

private trait PureDataTransfomStuff

trait ZaxTypes {
	type ZaxSMap = SMap[String, AnyRef]
	type ZaxFlag = Boolean
	type ZaxErr = (AnyRef,String)
	type ZaxResult=Either[ZaxErr,ZaxSMap]
}
trait ZaxlamMapReadHelper extends  ZaxTypes {

	// 3 possibilities:  a) good value b) no value c) bad value
	def getFlag(smap : ZaxSMap, key : String) : Either[ZaxErr, Option[ZaxFlag]] = {
		val flgOpt : Option[AnyRef] = smap.get(key)
		val eith = flgOpt match {
			case None => Right(None)
			case Some(jbool : JBool) => {
				val sbool = jbool.booleanValue()
				Right(Some(sbool))
			}
			case Some(other) => {
				val otherDesc = if (other != null) s"other[clz=${other.getClass.getName}]='${other.toString}'" else "NULL"
				Left(other, s"Expected ZaxFlag/Boolean at key=${key}, but got ${otherDesc}")
			}

		}
		eith
	}
}

trait ZaxlamMapXformer {
	def deepConvertJMapToSMap(jMap : JMap[String, AnyRef]) : SMap[String, AnyRef] = {
		// FIXME: This step does not deep-convert any JMaps or JLists lurking in the values.
		val shallowSMap: SMap[String, AnyRef] = jMap.asScala.toMap
		shallowSMap
	}
	def deepConvertSMapToJMap(sMap : SMap[String, AnyRef]) : JMap[String, AnyRef] = {
		// FIXME: This step does not deep-convert any SMaps or SLists lurking in the values.
		val shallowJMap = sMap.asJava
		shallowJMap
	}
}