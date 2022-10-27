package fun.gravax.dflow.zrunk

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper, SerializerProvider}

import org.apache.kafka.clients.producer.{KafkaProducer, Producer, ProducerConfig, ProducerRecord, RecordMetadata}
import org.apache.kafka.common.serialization.{StringSerializer, VoidSerializer}
import org.apache.kafka.connect.json.JsonSerializer

import java.util.Properties
import java.util.concurrent.Future

private trait ScalaKafkaWriterStuff

trait DummyPoster {
	val myKfkBootyUrl : String

	private lazy val myKVP = new KVPoster {
		override def getTopicName: String = "dum01"

		override val kfkBootyURL: String = myKfkBootyUrl

		override def getProducerBareName: String = "burpy_dum_cli_01b"
	}
	def postPracticeMsg : Unit = {
		val seqNum = 5012
		postXYZ("prac_X_" + seqNum, "prac_Y_" + seqNum, "prac_Z_" + seqNum)
	}
	def postXYZ(xTxt : String, yTxt : String, zTxt : String): Future[RecordMetadata] =  {
		val dumNode: JsonNode = mkNode(xTxt, yTxt, zTxt)
		println("Made node to post: " + dumNode.toPrettyString)
		val recKey = null // "fakeKey"
		// myKVP.sendWithKey(recKey, dumNode)
		val futMet: Future[RecordMetadata] = myKVP.sendWithoutKey(dumNode)
		futMet
	}

	private lazy val om = new ObjectMapper()
	def mkNode(xTxt : String, yTxt : String, zTxt : String) : JsonNode = {
		val objNode = om.createObjectNode()
		objNode.put("XTXT", xTxt)
		objNode.put("YTXT", yTxt)
		objNode.put("ZTXT", zTxt)
	}

	def close(): Unit = {
		myKVP.closeUnkeyedProducer
	}
}

trait KVPoster {
	val kfkBootyURL : String
	def getTopicName : String
	def getProducerBareName : String
	private lazy val myProd_withKey = mkProducer_withKey
	private lazy val myProd_noKey = mkProducer_noKey
	private def getProd_withKey : Producer[String, JsonNode] = myProd_withKey
	private def getProd_noKey : Producer[Void, JsonNode] = myProd_noKey

	def closeUnkeyedProducer: Unit = {
		myProd_noKey.close()
	}
	def closeKeyedProducer : Unit = {
		myProd_withKey.close()
	}
	private def mkProducer_withKey : Producer[String, JsonNode] = {
		val cliID = getProducerBareName + "-WKEY"
		val confProps  = mkProducerProps(cliID, true)
		val producer = new KafkaProducer[String, JsonNode](confProps);
		producer
	}
	private def mkProducer_noKey : Producer[Void, JsonNode] = {
		val cliID = getProducerBareName + "-NOKEY"
		val confProps  = mkProducerProps(cliID, false)
		val producer = new KafkaProducer[Void, JsonNode](confProps);
		producer
	}
	def sendWithKey(recKey : String, jsonNode : JsonNode) : Future[RecordMetadata] = {
		val producer = getProd_withKey
		val topicName = getTopicName
		println(s"Sending record with key=${recKey} to topic=${topicName}")
		val rec = new ProducerRecord[String, JsonNode](topicName, recKey, jsonNode);
		producer.send(rec);
	}
	def sendWithoutKey(jsonNode : JsonNode) : Future[RecordMetadata] = {
		val producer = getProd_noKey
		val topicName = getTopicName
		println(s"Sending unkeyed record to topic=${topicName}")
		val rec = new ProducerRecord[Void, JsonNode](topicName, null, jsonNode);
		producer.send(rec);
	}

	lazy val proppyMkr = new PropMaker {}
	def mkProducerProps(prodCliID : String, flg_strngKy : Boolean) : Properties = {
		val vclz0 = classOf[VoidSerializer]
		val ssclz0 = classOf[StringSerializer]
		val keySerClz = if (flg_strngKy) ssclz0 else vclz0
		val jsclz = classOf[JsonSerializer]
		val clientId : String = getProducerBareName
		val smap = Map[String, Object] (
			ProducerConfig.CLIENT_ID_CONFIG -> prodCliID,
			ProducerConfig.BOOTSTRAP_SERVERS_CONFIG -> kfkBootyURL,
			ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG -> keySerClz,
			ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG -> jsclz
		)
		println("KVPoster.ProducerProps as scala-map: " + smap)
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