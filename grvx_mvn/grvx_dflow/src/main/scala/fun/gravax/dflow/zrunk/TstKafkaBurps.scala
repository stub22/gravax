package fun.gravax.dflow.zrunk

import com.fasterxml.jackson.databind.JsonNode
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.PartitionInfo
import org.apache.kafka.common.serialization.{Serde, Serdes, StringDeserializer}
import org.apache.kafka.connect.json.{JsonDeserializer, JsonSerializer}
import org.apache.kafka.streams.kstream.{Consumed, KStream, Predicate, Printed}
import org.apache.kafka.streams.{KafkaStreams, StreamsBuilder, Topology}

import java.util
import java.util.Properties

object RunKafkaBurps {
	def main(args: Array[String]): Unit = {
		println("Burping Kafka")
		val bbb = new BBB{}
		bbb.doTopicDump
		bbb.startListeningForBurps
		bbb.mkSomeBurps
	}

}
// BBB = Big-Boss of Burpin
trait BBB {
	/*** Then in ksql console we can do:
	// "dum01" topic created from console with:
	//  docker-compose exec kafka kafka-topics  --bootstrap-server localhost:9092  --topic dum01 --replication-factor 1 --partitions 4 --create

	CREATE STREAM d01a WITH (KAFKA_TOPIC='dum01')
	But the above does not work if there are no messages in the topic.

	We get:    	Statement is missing the 'VALUE_FORMAT' property from the WITH clause.
				Either provide one or set a default via the 'ksql.persistence.default.format.value' config.

	https://docs.ksqldb.io/en/latest/developer-guide/ksqldb-reference/create-stream/

	So then we try:
	 	CREATE STREAM D2301A WITH (KAFKA_TOPIC='dum2301a', VALUE_FORMAT='json')
	or
	 	CREATE STREAM D2301A WITH (KAFKA_TOPIC='dum2301a', FORMAT='json')
	and we get:
		No columns supplied.

	This works:
	CREATE STREAM D2301A (XTXT VARCHAR KEY, YTXT VARCHAR, ZTXT VARCHAR) WITH (KAFKA_TOPIC='dum2301a', FORMAT='json')


	// INSERT INTO D2301A (XTXT, YTXT, ZTXT) VALUES ('is X 3a', 'was Y 3b', 'now Z 3c')
	// CREATE STREAM X2301A AS SELECT XTXT from D2301A EMIT CHANGES;
		Now gets:   The projection contains no value columns.
	So we change to:
		CREATE STREAM X2301A AS SELECT XTXT, ZTXT from D2301A EMIT CHANGES;

	// CREATE STREAM Y2301A WITH (VALUE_FORMAT='KAFKA') AS SELECT YTXT from D2301A EMIT CHANGES;
	Now gets:    	 Key missing from projection.
	The query used to build `Y2301A` must include the key column XTXT in its projection.

	Changed to this - note no format settings.
	  CREATE STREAM Y2301A AS SELECT XTXT, YTXT from D2301A EMIT CHANGES;

	*/
	val KFK_BOOTY_URL = "localhost:29092" // not "kafka:9092"

	val DUM_TOPIC_NAME = "D2301A"
	val X_TOPIC_NAME = "X2301A"
	val Y_TOPIC_NAME = "Y2301A"
	def doTopicDump : Unit = {
		val topDump = new TopicDumper {
			override val kfkBootyURL: String = KFK_BOOTY_URL
		}
		topDump.dumpTopicList
	}
	def startListeningForBurps : Unit = {
		val bb = new BossBurpListener {
			override val kfkBootyURL: String = KFK_BOOTY_URL
		}

		/*
		 "Data should be null for a VoidDeserializer."
		 Deserialization exception handler is set to fail upon a deserialization error.
		 If you would rather have the streaming pipeline continue after a deserialization error, please set the
		 default.deserialization.exception.handler appropriately.
		 */

				val flg_dumAsJson = true
				val flg_dumHasKy = true
				if (flg_dumAsJson) {
					if (flg_dumHasKy) {
						val jsonStrmDum01_strngKy: KStream[String, JsonNode] = bb.mkRecordStream_strngKey(DUM_TOPIC_NAME)
						bb.mkJsonDumper_withKey(jsonStrmDum01_strngKy, "json-dum01-withKey", true)

						val jsonStrmX01A_withKey : KStream[String, JsonNode] = bb.mkRecordStream_strngKey(X_TOPIC_NAME)
						bb.mkJsonDumper_withKey(jsonStrmX01A_withKey, "json-x01a-strm-withKey", true)

					} else {
						val jsonStrmDum01_noKy: KStream[Void, JsonNode] = bb.mkRecordStream_noKey(DUM_TOPIC_NAME)
						bb.mkJsonDumper_noKey(jsonStrmDum01_noKy, "json-dum01-noKey", true)

						val jsonStrmX01A_noKy : KStream[Void, JsonNode] = bb.mkRecordStream_noKey(X_TOPIC_NAME)
						bb.mkJsonDumper_noKey(jsonStrmX01A_noKy, "json-x01a-strm-noKey", true)
					}
				} else {
					val strmDum01: KStream[Void, String] = bb.mkStreamWithoutKey(DUM_TOPIC_NAME)
					bb.mkTextDumpers(strmDum01, "dum01-strm", false)
				}

				// FIXME:  Make a withKey version of mkTextDumpers
				val strmY01A : KStream[Void, String] = bb.mkStreamWithoutKey(Y_TOPIC_NAME)
				bb.mkTextDumpers(strmY01A, "y01a-strm-noKey", false)



		// you can also print using the `print` operator
		// stream.print(Printed.<String, String>toSysOut().withLabel("source"));
		bb.launchBurpListenerStreams
		println("END of Burp-Start")
	}
	def mkSomeBurps : Unit = {
		val dumPost = new DummyPoster {
			override val myKfkBootyUrl = KFK_BOOTY_URL
			override val myDumTopicNm = DUM_TOPIC_NAME
		}
		dumPost.postPracticeMsg
		dumPost.close()
	}
}
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.clients.consumer.ConsumerConfig

trait PropMaker {
	def scalaMapToProps(smap : Map[String, Object]) : Properties = {
		import scala.jdk.CollectionConverters._
		val jmap: util.Map[String, Object] = smap.asJava
		javaMapToProps(jmap)
	}
	def javaMapToProps(jmap: util.Map[String, Object]) : Properties = {
		val jup = new Properties()
		jup.putAll(jmap)
		jup
	}
}
trait BossBurpListener {

	val kfkBootyURL : String

	lazy val myPMkr = new PropMaker {}
	def getListenerProps : Properties = {

		val listenerPropsSMap = Map[String, Object] (
			StreamsConfig.APPLICATION_ID_CONFIG -> "brp-listen-app",
			StreamsConfig.BOOTSTRAP_SERVERS_CONFIG -> kfkBootyURL,
			ConsumerConfig.AUTO_OFFSET_RESET_CONFIG -> "earliest"

		)
		myPMkr.scalaMapToProps(listenerPropsSMap)
	}

	def UNUSED_mkTopologyFromScratch : Topology = ???  // Unused bc We get topology from a streams-builder

	// Stateful code:  This builder holds the topology we are building up.
	private lazy val myStrmsBldr = new StreamsBuilder();
	// https://kafka.apache.org/27/javadoc/org/apache/kafka/streams/StreamsBuilder.html
	def getStreamsBuilder : StreamsBuilder = myStrmsBldr


	// Deserialization fails if a key unexpectedly appears.
	def mkStreamWithoutKey(topicNm : String) : KStream[Void, String] = {
		// This adds a stream processor to the topology we are building up, which eventually gets
		// realized during launchStreamsProducerApp method.

		// This construction step requires only the BUILDER
		val builder = getStreamsBuilder
		val naiveStrmNoKey: KStream[Void, String] = builder.stream(topicNm)
		if (false) {
			val inStrm: KStream[String, String] = builder.stream(topicNm)
			val filtPred = mkStringKeyFilterPred[String]
			val fStrm: KStream[String, String] = inStrm.filter(filtPred)
			val x = ???
			val ttStrm = fStrm.transform[Void, String](x)
			fStrm
			???
		} else naiveStrmNoKey
		//   KStream<K, V> = return this.stream((Collection)Collections.singleton(topic))

	}
	def mkStringKeyFilterPred[VTI] : Predicate[String, VTI] = {
		new Predicate[String, VTI]() {
			override def test(k: String, v: VTI): Boolean = {
				k == null
			}
		}
	}
	def getPlainJsonSerde: Serde[JsonNode] = {
		val jsonSer = new JsonSerializer();
		val jsonDeser = new JsonDeserializer();
		val jsonSerde: Serde[JsonNode] = Serdes.serdeFrom(jsonSer, jsonDeser);
		jsonSerde
	}
	def getVoidSerde: Serde[Void] = Serdes.Void()
	def getStringSerde: Serde[String] = Serdes.String()
	def mkRecordStream_noKey(topicNm : String) : KStream[Void, JsonNode] = {
/*
public <K,V> KStream<K,V> stream(String topic,    Consumed<K,V> consumed)
Create a KStream from the specified topic. The "auto.offset.reset" strategy, TimestampExtractor,
key and value deserializers are defined by the options in Consumed are used.
Note that the specified input topic must be partitioned by key. If this is not the case it is the user's responsibility
to repartition the data before any key based operation (like aggregation or join) is applied to the returned KStream.
Parameters:
topic - the topic names; cannot be null
consumed - the instance of Consumed used to define optional parameters
 */

		val builder = getStreamsBuilder
		// val jsonNodeSerde = Serdes.
		val cnsmrCnf: Consumed[Void, JsonNode] = Consumed.`with`(getVoidSerde, getPlainJsonSerde)
		val strm: KStream[Void, JsonNode] = builder.stream(topicNm, cnsmrCnf)
		strm
	}
	def mkRecordStream_strngKey(topicNm : String) : KStream[String, JsonNode] = {
		val builder = getStreamsBuilder
		// val jsonNodeSerde = Serdes.
		val cnsmrCnf: Consumed[String, JsonNode] = Consumed.`with`(getStringSerde, getPlainJsonSerde)
		val strm: KStream[String, JsonNode] = builder.stream(topicNm, cnsmrCnf)
		strm
	}

	def mkTextDumpers(strm : KStream[Void, String], labelTxt : String, flg_more : Boolean) : Unit = {
		/*
[hey_p_label]: null, [B@460b2748
[hey_p_label]: null, [B@2cba6c40
		 */
		val noResult = strm.print(Printed.toSysOut[Void, String].withLabel(labelTxt))
		// val noResult = strm.print(Printed.toSysOut.withLabel("oi_p_label"))

		/*
Exception in thread "brp-prd-app-780998e2-c8f1-4e4b-8fe4-d23dc614806c-StreamThread-1"
org.apache.kafka.streams.errors.StreamsException: ClassCastException invoking Processor.
Do the Processor's input types match the deserialized types?
Check the Serde setup and change the default Serdes in StreamConfig or provide correct Serdes via method parameters.
Make sure the Processor can accept the deserialized input of type key: unknown because key is null, and value: [B.
Note that although incorrect Serdes are a common cause of error, the cast exception might have another cause
(in user code, for example). For example, if a processor wires in a store, but casts the generics incorrectly,
a class cast exception could be raised during processing, but the cause would not be wrong Serdes.
Caused by: java.lang.ClassCastException: [B cannot be cast to java.lang.String
V.toString=${v.toString}

void foreach(ForeachAction<? super K,? super V> action)
Perform an action on each record of KStream. This is a stateless record-by-record operation
(cf. process(ProcessorSupplier, String...)). Note that this is a terminal operation that returns void.
Terminal operation. Performs a stateless action on each record. (details)
You would use foreach to cause side effects based on the input data (similar to peek) and then stop further processing
of the input data (unlike peek, which is not a terminal operation).
Note on processing guarantees: Any side effects of an action (such as writing to external systems) are
not trackable by Kafka, which means they will typically not benefit from Kafka’s processing guarantees.
		*/
		if (flg_more) {
			strm.foreach((k, v) => {
				println(s"Got record on strm=${strm} with k=${k} and v=${v}")
			})
		}

	}
	private def dumpJsonNodeDeets(jn : JsonNode, labelTxt : String) : Unit = {
		val xtxt: JsonNode = jn.get("XTXT")
		val xtt : String = if (xtxt != null) xtxt.asText("OOPS") else "NULL"
		val ytxt: JsonNode = jn.get("YTXT")
		val ytt : String = if (ytxt != null) ytxt.asText("OUCH") else "NULL"

		println(s" NODE DEETS ON ${labelTxt} are xtxt=${xtxt}, xtt=${xtt}, ytxt=${ytxt}, ytt=${ytt}")
	}
	def mkJsonDumper_noKey(strm : KStream[Void, JsonNode], labelTxt : String, flg_more : Boolean) : Unit = {
		val noResult = strm.print(Printed.toSysOut[Void, JsonNode].withLabel(labelTxt))
		if (flg_more) {
			strm.foreach((k, v) => {
				// V.class = com.fasterxml.jackson.databind.node.ObjectNode
				println(s"dumper_noKey ${labelTxt} got record on strm=${strm} K=${k}, V.class=${v.getClass} V.toPrettyString=\n${v.toPrettyString}")
				dumpJsonNodeDeets(v, labelTxt)
			})
		}
	}
	def mkJsonDumper_withKey(strm : KStream[String, JsonNode], labelTxt : String, flg_more : Boolean) : Unit = {
		val noResult = strm.print(Printed.toSysOut[String, JsonNode].withLabel(labelTxt))
		if (flg_more) {
			strm.foreach((k, v) => {
				// V.class = com.fasterxml.jackson.databind.node.ObjectNode
				println(s"dumper_withKey ${labelTxt} got record on strm=${strm} K=${k}, V.class=${v.getClass} V.toPrettyString=\n${v.toPrettyString}")
				dumpJsonNodeDeets(v, labelTxt)
			})
		}
	}

	def launchBurpListenerStreams : KafkaStreams = {
		val bldr: StreamsBuilder = getStreamsBuilder
		val listenConfProps: Properties = getListenerProps
		println("launchBurpListenerStreams found listener conf properties: " + listenConfProps)
		launchListenerStreams(bldr, listenConfProps)
	}
	def launchListenerStreams(strmsBldr : StreamsBuilder, listenConfProps : Properties) : KafkaStreams = {

		val topo: Topology = strmsBldr.build()
		println("launchListenerStreams found built topology: " + topo.describe())

		val streams = new KafkaStreams(topo, listenConfProps);
		println("launchListenerStreams is starting KafkaStreams processor");
		val ssRsltIsUnit: Unit = streams.start();
		println("launchListenerStreams is adding shutdown hook");
		// close Kafka Streams when the JVM shuts down (e.g. SIGTERM)
		val closeThread = new Thread() {
			override def run(): Unit = {
				println("our producer shutdown hook is running, will now call streams.close")
				streams.close()
				println("our shutdown hook finished calling streams.close().  Bye!")
			}
		}
		Runtime.getRuntime().addShutdownHook(closeThread);
		streams
	}

}

trait TopicDumper {
	val kfkBootyURL : String

	def getConsumerProps : util.Map[String, Object] = {
		// import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG

		val sdclz0 = classOf[StringDeserializer]
		val sdclz1 = classOf[Serdes.StringSerde] // class org.apache.kafka.common.serialization.Serdes$StringSerde is not an instance of org.apache.kafka.common.serialization.Deserializer

		val consumerProps = Map[String, Object](
			ConsumerConfig.GROUP_ID_CONFIG -> "burp-consumer-grp",
			ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG -> kfkBootyURL ,
			ConsumerConfig.AUTO_OFFSET_RESET_CONFIG -> "earliest",
			ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG -> sdclz0,
			ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG -> sdclz0
		)
		// StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, ssclz,)
		import scala.jdk.CollectionConverters._
		consumerProps.asJava
	}
	def dumpTopicList : Unit = {
		val consumerProps = getConsumerProps
		val topicListConsumer = new KafkaConsumer[String, String](consumerProps);
		val topics: util.Map[String, util.List[PartitionInfo]] = topicListConsumer.listTopics();
		println("listTopics - count: " + topics.size())
		println("listTopics - entries:  " + topics)
		import scala.jdk.CollectionConverters._
		topics.asScala.foreach { case (key, partList) => println(s"key=${key}, partList=${partList}") }
		topicListConsumer.close();
	}
}
/***
			KEY_DESERIALIZER_CLASS_CONFIG -> classOf[KafkaAvroDeserializer],
			VALUE_DESERIALIZER_CLASS_CONFIG -> classOf[KafkaAvroDeserializer],
			SCHEMA_REGISTRY_URL_CONFIG -> "http://schema-registry:8081"

 *
public static void main(String[] args) {
    // the builder is used to construct the topology
    Topology topology = new Topology();

    topology.addSource("UserSource", "users");
    topology.addProcessor("SayHello", SayHelloProcessor::new, "UserSource");

    // set the required properties for running Kafka Streams
    Properties config = new Properties();
    config.put(StreamsConfig.APPLICATION_ID_CONFIG, "dev2");
     config.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:29092");
    config.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
    config.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.Void().getClass());
    config.put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, Serdes.String().getClass());

    // build the topology and start streaming!
    KafkaStreams streams = new KafkaStreams(topology, config);
    System.out.println("Starting streams");
    streams.start();

    // close Kafka Streams when the JVM shuts down (e.g. SIGTERM)
    Runtime.getRuntime().addShutdownHook(new Thread(streams::close));
  }

 DslExample {

  public static void main(String[] args) {
    // the builder is used to construct the topology
    StreamsBuilder builder = new StreamsBuilder();

    // read from the source topic, "users"
    KStream<Void, String> stream = builder.stream("chaw");

    // for each record that appears in the source topic,
    // print the value
    stream.foreach(
        (key, value) -> {
          System.out.println("(DSL) Hello, " + value);
        });

    // you can also print using the `print` operator
    // stream.print(Printed.<String, String>toSysOut().withLabel("source"));

    // set the required properties for running Kafka Streams
    Properties config = new Properties();
    config.put(StreamsConfig.APPLICATION_ID_CONFIG, "dev1");
    config.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:29092");
    config.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
    config.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.Void().getClass());
    config.put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, Serdes.String().getClass());

    System.out.println("Building streams obj");
    // build the topology and start streaming
    KafkaStreams streams = new KafkaStreams(builder.build(), config);
    System.out.println("Starting streams");
    streams.start();
    System.out.println("Adding shutdown hook");
    // close Kafka Streams when the JVM shuts down (e.g. SIGTERM)
    Runtime.getRuntime().addShutdownHook(new Thread(streams::close));
    System.out.println("End of main");
  }
 */

     /**
 * A producer is instantiated by providing a set of key-value pairs as configuration, a key and a value {@link Serializer}.
 * Valid configuration strings are documented <a href="http://kafka.apache.org/documentation.html#producerconfigs">here</a>.
 * Values can be either strings or Objects of the appropriate type (for example a numeric configuration would accept
 * either the string "42" or the integer 42).
 * <p>
 * Note: after creating a {@code KafkaProducer} you must always {@link #close()} it to avoid resource leaks.
 * @param configs   The producer configs
 * @param keySerializer  The serializer for key that implements {@link Serializer}. The configure() method won't be
 *                       called in the producer when the serializer is passed in directly.
 * @param valueSerializer  The serializer for value that implements {@link Serializer}. The configure() method won't
 *                         be called in the producer when the serializer is passed in directly.

    public KafkaProducer(Map<String, Object> configs, Serializer<K> keySerializer, Serializer<V> valueSerializer) {

 */