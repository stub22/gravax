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

	val myDumTopicNm : String

	private lazy val myKVP = new KVPoster {
		override def getTopicName: String =  myDumTopicNm // "dum01"

		override val kfkBootyURL: String = myKfkBootyUrl

		override def getProducerBareName: String = "burpy_dum_cli_01b"
	}
	def postPracticeMsg : Unit = {
		val seqNum = 5033
		// postXYZ_noKey
		postXYZ_withStringKey("prac_X_" + seqNum, "prac_Y_" + seqNum, "prac_Z_" + seqNum)
	}
	val flag_pwk = true
	private def postXYZ_noKey(xTxt : String, yTxt : String, zTxt : String): Future[RecordMetadata] =  {
		val dumNode: JsonNode = mkNode(xTxt, yTxt, zTxt)
		println("Made node to post: " + dumNode.toPrettyString)
		val recKey = null // "fakeKey"
		// myKVP.sendWithStringKey(recKey, dumNode)
		val futMet: Future[RecordMetadata] = myKVP.sendWithoutKey(dumNode)
		futMet
	}
	private def postXYZ_withStringKey(xTxt : String, yTxt : String, zTxt : String): Future[RecordMetadata] = {
		val dumNode: JsonNode = mkNode(xTxt, yTxt, zTxt)
		println("Made node to post: " + dumNode.toPrettyString)
		val futMet: Future[RecordMetadata] = myKVP.sendWithStringKey(xTxt, dumNode)
		futMet
	}
	private def postXYZ_withJsonKey(xTxt : String, yTxt : String, zTxt : String): Future[RecordMetadata] = {
		val dumNode: JsonNode = mkNode(xTxt, yTxt, zTxt)
		println("Made node to post: " + dumNode.toPrettyString)
		val futMet: Future[RecordMetadata] = myKVP.sendWithStringKey(xTxt, dumNode)
		futMet
	}
	private lazy val om = new ObjectMapper()
	private def mkNode(xTxt : String, yTxt : String, zTxt : String) : JsonNode = {
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
	private lazy val myProd_withStringKey = mkProducer_withStringKey
	private lazy val myProd_noKey = mkProducer_noKey
	private def getProd_withStringKey : Producer[String, JsonNode] = myProd_withStringKey
	private def getProd_noKey : Producer[Void, JsonNode] = myProd_noKey

	def closeUnkeyedProducer: Unit = {
		myProd_noKey.close()
	}
	def closeStringKeyedProducer : Unit = {
		myProd_withStringKey.close()
	}
	private def mkProducer_withStringKey : Producer[String, JsonNode] = {
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
	def sendWithStringKey(recKey : String, jsonNode : JsonNode) : Future[RecordMetadata] = {
		val producer = getProd_withStringKey
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

	private lazy val proppyMkr = new PropMaker {}
	private def mkProducerProps(prodCliID : String, flg_strngKy : Boolean) : Properties = {
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


https://kafka.apache.org/27/javadoc/index.html?org/apache/kafka/clients/producer/KafkaProducer.html

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