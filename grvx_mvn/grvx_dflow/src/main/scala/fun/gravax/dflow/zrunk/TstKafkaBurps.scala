package fun.gravax.dflow.zrunk

import com.fasterxml.jackson.databind.JsonNode
import org.apache.kafka.clients.consumer.ConsumerConfig.{VALUE_DESERIALIZER_CLASS_CONFIG, _}
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.PartitionInfo
import org.apache.kafka.common.serialization.{Serde, Serdes, StringDeserializer}
import org.apache.kafka.connect.json.{JsonDeserializer, JsonSerializer}
import org.apache.kafka.streams.kstream.{Consumed, KStream, Printed}
import org.apache.kafka.streams.{KafkaStreams, StreamsBuilder, Topology}

import java.util
import java.util.Properties

object RunKafkaBurps {
	def main(args: Array[String]): Unit = {
		println("Burping Kafka")
		doTopicDump
		doSomeBurpin

	}
	val KFK_BOOTY_URL = "localhost:29092" // not "kafka:9092"

	def doTopicDump : Unit = {
		val topDump = new TopicDumper {
			override val kfkBootyURL: String = KFK_BOOTY_URL
		}
		topDump.dumpTopicList
	}
	def doSomeBurpin : Unit = {
		val bb = new BossBurper {
			override val kfkBootyURL: String = KFK_BOOTY_URL
		}

		val strmDum01: KStream[Void, String] = bb.mkWeakStream("dum01")
		println("Got strmDum01: " + strmDum01)
		bb.mkTextDumpers(strmDum01, "dum01-strm", false)

		val strmY01A : KStream[Void, String] = bb.mkWeakStream("Y01A")

		bb.mkTextDumpers(strmY01A, "y01a-strm", false)

		val strmX01A : KStream[Void, JsonNode] = bb.mkRecordStream("X01A")

		bb.mkJsonDumper(strmX01A, "x01a-strm", true)


		// you can also print using the `print` operator
		// stream.print(Printed.<String, String>toSysOut().withLabel("source"));
		bb.launchStreamsProducerApp
		println("END of Burp-Start")
	}

}
import org.apache.kafka.streams.StreamsConfig
import org.apache.kafka.clients.consumer.ConsumerConfig

trait BossBurper {

	val kfkBootyURL : String

	def getProducerProps : Properties = {

		val producerPropsSMap = Map (
			StreamsConfig.APPLICATION_ID_CONFIG -> "brp-prd-app",
			StreamsConfig.BOOTSTRAP_SERVERS_CONFIG -> kfkBootyURL,
			ConsumerConfig.AUTO_OFFSET_RESET_CONFIG -> "earliest"
		)
		import scala.jdk.CollectionConverters._
		val ppJMap: util.Map[String, String] = producerPropsSMap.asJava
		val jup = new Properties()
		jup.putAll(ppJMap)
		jup
	}

	def UNUSED_mkTopologyFromScratch : Topology = ???  // Unused bc We get topology from a streams-builder

	// Stateful code:  This builder holds the topology we are building up.
	private lazy val myStrmsBldr = new StreamsBuilder();
	// https://kafka.apache.org/27/javadoc/org/apache/kafka/streams/StreamsBuilder.html
	def getStreamsBuilder : StreamsBuilder = myStrmsBldr

	def mkWeakStream(topicNm : String) : KStream[Void, String] = {
		// This adds a stream processor to the topology we are building up, which eventually gets
		// realized during launchStreamsProducerApp method.

		// This construction step requires only the BUILDER
		val builder = getStreamsBuilder
		val strm: KStream[Void, String] = builder.stream(topicNm)
		//   KStream<K, V> = return this.stream((Collection)Collections.singleton(topic))
		strm
	}

	def mkRecordStream(topicNm : String) : KStream[Void, JsonNode] = {
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

		val jsonSer = new JsonSerializer();
		val jsonDeser = new JsonDeserializer();
		val jsonSerde: Serde[JsonNode] = Serdes.serdeFrom(jsonSer, jsonDeser);
		val stringSerde: Serde[String] = Serdes.String()
		val voidSerde = Serdes.Void()
		val builder = getStreamsBuilder
		// val jsonNodeSerde = Serdes.
		val cnsmrCnf = Consumed.`with`(voidSerde, jsonSerde)
		val strm: KStream[Void, JsonNode] = builder.stream(topicNm, cnsmrCnf)
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
		*/
		if (flg_more) {
			strm.foreach((k, v) => {
				println(s"Got record on strm=${strm}") //  K.class=${k.getClass}, V.class=${v.getClass}")
			})
		}

	}
	def mkJsonDumper(strm : KStream[Void, JsonNode], labelTxt : String, flg_more : Boolean) : Unit = {
		val noResult = strm.print(Printed.toSysOut[Void, JsonNode].withLabel(labelTxt))
		if (flg_more) {
			strm.foreach((k, v) => {
				println(s"Got record on strm=${strm} K=${k}, V.class=${v.getClass} V.toPrettyString=\n${v.toPrettyString}")
			})
		}
	}

	def launchStreamsProducerApp : KafkaStreams = {
		val bldr = getStreamsBuilder
		val topo: Topology = bldr.build()
		println("Built topology: " + topo.describe())
		val prodConf = getProducerProps
		val streams = new KafkaStreams(topo, prodConf);
		println("Starting streams");
		streams.start();
		println("Adding shutdown hook");
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
			KEY_DESERIALIZER_CLASS_CONFIG -> sdclz0,
			VALUE_DESERIALIZER_CLASS_CONFIG -> sdclz0
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