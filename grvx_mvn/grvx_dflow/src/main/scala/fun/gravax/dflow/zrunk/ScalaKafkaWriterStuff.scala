package fun.gravax.dflow.zrunk

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, JsonPointer, JsonToken, ObjectCodec}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper, SerializerProvider}
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.node.JsonNodeType
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.kafka.clients.producer.{KafkaProducer, Producer, ProducerRecord}
import org.apache.kafka.common.serialization.{StringSerializer, VoidSerializer}
import org.apache.kafka.connect.json.JsonSerializer

import java.util
import java.util.Properties

private trait ScalaKafkaWriterStuff

trait DummyPoster {
	val myKfkBootyUrl : String

	private lazy val myKVP = new KVPoster {
		override def getTopicName: String = "dum01"

		override val kfkBootyURL: String = myKfkBootyUrl
	}
	def postPracticeMsg : Unit = {
		val seqNum = 5009
		postXYZ("prac_X_" + seqNum, "prac_Y_" + seqNum, "prac_Z_" + seqNum)
	}
	def postXYZ(xTxt : String, yTxt : String, zTxt : String): Unit =  {
		val dumNode: JsonNode = mkNode(xTxt, yTxt, zTxt)
		println("Made node to post: " + dumNode.toPrettyString)
		val recKey = null // "fakeKey"
		// myKVP.sendWithKey(recKey, dumNode)
		myKVP.sendWithoutKey(dumNode)
	}

	lazy val om = new ObjectMapper()
	def mkNode(xTxt : String, yTxt : String, zTxt : String) : JsonNode = {
		val objNode = om.createObjectNode()
		objNode.put("XTXT", xTxt)
		objNode.put("YTXT", yTxt)
		objNode.put("ZTXT", zTxt)
	}
}

trait KVPoster {
	val kfkBootyURL : String
	def getTopicName : String
	private lazy val myProd_withKey = mkProducer_withKey
	private lazy val myProd_noKey = mkProducer_noKey
	private def getProd_withKey : Producer[String, JsonNode] = myProd_withKey
	private def getProd_noKey : Producer[Void, JsonNode] = myProd_noKey
	private def mkProducer_withKey : Producer[String, JsonNode] = {
		val confProps  = mkProducerProps(true)
		val producer = new KafkaProducer[String, JsonNode](confProps);
		producer
	}
	private def mkProducer_noKey : Producer[Void, JsonNode] = {
		val confProps  = mkProducerProps(false)
		val producer = new KafkaProducer[Void, JsonNode](confProps);
		producer
	}
	def sendWithKey(recKey : String, jsonNode : JsonNode) : Unit = {
		val producer = getProd_withKey
		val rec = new ProducerRecord[String, JsonNode](getTopicName, recKey, jsonNode);
		producer.send(rec);
	}
	def sendWithoutKey(jsonNode : JsonNode) : Unit = {
		val producer = getProd_noKey
		val rec = new ProducerRecord[Void, JsonNode](getTopicName, null, jsonNode);
		producer.send(rec);
	}

	lazy val proppyMkr = new PropMaker {}
	def mkProducerProps(flg_strngKy : Boolean) : Properties = {
		val vclz0 = classOf[VoidSerializer]
		val ssclz0 = classOf[StringSerializer]
		val keySerClz = if (flg_strngKy) ssclz0 else vclz0
		val jsclz = classOf[JsonSerializer]
		val clientId : String = "burp-producer-B35_" + flg_strngKy.toString
		val smap = Map[String, Object] (
			ProducerConfig.CLIENT_ID_CONFIG -> clientId,
			ProducerConfig.BOOTSTRAP_SERVERS_CONFIG -> kfkBootyURL,
			ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG -> keySerClz,
			ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG -> jsclz
		)
		println("ProducerProps as scala-map: " + smap)
		proppyMkr.scalaMapToProps(smap)
	}
}
/*
ProducerRecord(String topic, Integer partition, K key, V value)
Creates a record to be sent to a specified topic and partition
ProducerRecord(String topic, Integer partition, K key, V value, Iterable<Header> headers)
Creates a record to be sent to a specified topic and partition
ProducerRecord(String topic, Integer partition, Long timestamp, K key, V value)
Creates a record with a specified timestamp to be sent to a specified topic and partition
ProducerRecord(String topic, Integer partition, Long timestamp, K key, V value, Iterable<Header> headers)
Creates a record with a specified timestamp to be sent to a specified topic and partition
ProducerRecord(String topic, K key, V value)
Create a record to be sent to Kafka
ProducerRecord(String topic, V value)
Create a record with no key
 */