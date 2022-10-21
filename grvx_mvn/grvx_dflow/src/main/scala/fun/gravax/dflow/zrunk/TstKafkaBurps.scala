package fun.gravax.dflow.zrunk

import org.apache.kafka.clients.consumer.ConsumerConfig._
import org.apache.kafka.streams.kstream.KStream
import org.apache.kafka.streams.{KafkaStreams, StreamsBuilder, Topology}

import java.util
import java.util.Properties

object RunKafkaBurps {
	def main(args: Array[String]): Unit = {
		println("Burping Kafka")
		doSomeBurpin

	}
	def doSomeBurpin : Unit = {
		val bb = new BossBurper {}
		val ws = bb.mkWeakStream("fak01")
		println("Got wkStrm: " + ws)
		// you can also print using the `print` operator
		// stream.print(Printed.<String, String>toSysOut().withLabel("source"));
		bb.launchStreamsProducerApp
		println("END of Burp-Start")
	}
}

trait BossBurper {
	def getConsumerProps : Map[String, Object] = {

		import org.apache.kafka.clients.consumer.ConsumerConfig._
		// import io.confluent.kafka.serializers.AbstractKafkaSchemaSerDeConfig.SCHEMA_REGISTRY_URL_CONFIG

		val consumerProps: Map[String, Object] = Map(
			GROUP_ID_CONFIG -> "burp-consumer-grp",
			BOOTSTRAP_SERVERS_CONFIG -> "kafka:9092")
		consumerProps
	}

	def getProducerProps : Properties = {
		import org.apache.kafka.streams.StreamsConfig._
		val producerPropsSMap = Map (
			APPLICATION_ID_CONFIG -> "brpPrd",
			BOOTSTRAP_SERVERS_CONFIG -> "localhost:29092",
			AUTO_OFFSET_RESET_CONFIG -> "earliest"
		)
		import scala.jdk.CollectionConverters._
		val ppJMap: util.Map[String, String] = producerPropsSMap.asJava
		val jup = new Properties()
		jup.putAll(ppJMap)
		jup
	}

	def mkTopology : Topology = ???

	private lazy val myStrmsBldr = new StreamsBuilder();
	def getStreamsBuilder : StreamsBuilder = myStrmsBldr

	def mkWeakStream(topicNm : String) : KStream[Void, String] = {
		// Requires only the BUILDER
		val builder = getStreamsBuilder
		val strm: KStream[Void, String] = builder.stream(topicNm)
		//   KStream<K, V> = return this.stream((Collection)Collections.singleton(topic))
		strm
	}

	def launchStreamsProducerApp : KafkaStreams = {
		val bldr = getStreamsBuilder
		val topo: Topology = bldr.build()
		val prodConf = getProducerProps
		val streams = new KafkaStreams(topo, prodConf);
		System.out.println("Starting streams");
		streams.start();
		System.out.println("Adding shutdown hook");
		// close Kafka Streams when the JVM shuts down (e.g. SIGTERM)
		val closeThread = new Thread() {
			override def run(): Unit = {
				System.out.println("our producer shutdown hook is running, will now call streams.close")
				streams.close()
				System.out.println("our shutdown hook finished calling streams.close().  Bye!")
			}
		}
		Runtime.getRuntime().addShutdownHook(closeThread);
		streams
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