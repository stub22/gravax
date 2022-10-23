package fun.gravax.dflow.zrunk

private trait ScalaKafkaExampleStuff

/*
example Code copied and adapted from
https://github.com/kubinio123/hands-on-kafka-streams
 */
object exCarDomain {

	case class CarId(value: Int)

	case class CarSpeed(value: Int)
	case class CarEngine(rpm: Int, fuelLevel: Double)
	case class CarLocation(locationId: LocationId)

	case class DriverNotification(msg: String)

	case class LocationId(city: String, street: String)
	case class LocationData(speedLimit: Int, trafficVolume: TrafficVolume.Value, gasStationNearby: Boolean)

	object TrafficVolume extends Enumeration {
		val Low, Medium, High = Value
	}
}
object exRandomData {
	import exCarDomain._
	import scala.util.Random

	private val carIds = Seq(1, 2)
	private val cities = Seq("Wroclaw", "Cracow")
	private val streets = Seq("Sezamowa", "Tunelowa")

	def carSpeed: Seq[(CarId, CarSpeed)] =
		for {
			carId <- carIds
			speed = Random.between(5, 10) * 10
		} yield CarId(carId) -> CarSpeed(speed)

	def carEngine: Seq[(CarId, CarEngine)] =
		for {
			carId <- carIds
			rpm = Random.between(25, 35) * 100
			fuelLevel = (math floor Random.between(0d, 1d) * 100) / 100
		} yield CarId(carId) -> CarEngine(rpm, fuelLevel)

	def carLocation: Seq[(CarId, CarLocation)] =
		for {
			carId <- carIds
			city = cities(Random.nextInt(cities.size))
			street = streets(Random.nextInt(streets.size))
		} yield CarId(carId) -> CarLocation(LocationId(city, street))

	def locationData: Seq[(LocationId, LocationData)] =
		for {
			city <- cities
			street <- streets
			speedLimit = Random.between(3, 7) * 10
			trafficVolume = TrafficVolume(Random.nextInt(TrafficVolume.maxId))
			gasStationNearby = Random.nextBoolean()
		} yield LocationId(city, street) -> LocationData(speedLimit, trafficVolume, gasStationNearby)
}

object exDriverNotifierData {
	import exCarDomain._
	case class CarData(speed: Option[CarSpeed], engine: Option[CarEngine], location: Option[CarLocation])

	object CarData {
		val empty: CarData = CarData(None, None, None)
	}

	case class CarAndLocationData(carData: CarData, locationData: LocationData)
}

object exDriverNotifications {
	import exCarDomain._
	import exDriverNotifierData._
	def apply(data: CarAndLocationData): List[DriverNotification] =
		List(checkSpeed, checkTrafficVolume, checkEngineRPM, checkFuelLevel).flatten(_.lift(data))

	private val checkSpeed: PartialFunction[CarAndLocationData, DriverNotification] = {
		case CarAndLocationData(CarData(Some(speed), _, _), LocationData(speedLimit, _, _)) if speed.value > speedLimit =>
			DriverNotification(s"Slow down, speed limit $speedLimit")
	}

	private val checkTrafficVolume: PartialFunction[CarAndLocationData, DriverNotification] = {
		case CarAndLocationData(CarData(_, _, Some(location)), LocationData(_, TrafficVolume.High, _)) =>
			DriverNotification(s"High traffic ahead on ${location.locationId.street} street")
	}

	private val checkEngineRPM: PartialFunction[CarAndLocationData, DriverNotification] = {
		case CarAndLocationData(CarData(_, Some(engine), _), _) if engine.rpm > HighRPM => DriverNotification("Shift up a gear")
	}

	private val checkFuelLevel: PartialFunction[CarAndLocationData, DriverNotification] = {
		case CarAndLocationData(CarData(_, Some(engine), _), LocationData(_, _, gasStationNearby))
			if engine.fuelLevel <= FuelReserve && gasStationNearby =>
			DriverNotification("Low fuel level, navigate to nearest gas station?")
	}

	private val HighRPM = 3000
	private val FuelReserve = 0.2
}

/***
object AvroSerdes {

	private val props = Map("schema.registry.url" -> "http://schema-registry:8081")

	implicit def keySerde[K >: Null](implicit krf: KeyRecordFormat[K]): Serde[K] = {
		val avroKeySerde = new GenericAvroSerde
		avroKeySerde.configure(props.asJava, true)
		avroKeySerde.forCaseClass[K]
	}

	implicit def valueSerde[V >: Null](implicit vrf: ValueRecordFormat[V]): Serde[V] = {
		val avroValueSerde = new GenericAvroSerde
		avroValueSerde.configure(props.asJava, false)
		avroValueSerde.forCaseClass[V]
	}

	implicit class CaseClassSerde(inner: Serde[GenericRecord]) {
		def forCaseClass[T >: Null](implicit rf: RecordFormat[T]): Serde[T] = {
			Serdes.fromFn(
				(topic, data) => inner.serializer().serialize(topic, rf.to(data)),
				(topic, bytes) => Option(rf.from(inner.deserializer().deserialize(topic, bytes)))
			)
		}
	}
}


kafka-topics --create --bootstrap-server kafka:9092 --partitions 2 --replication-factor 1 --topic car-speed
kafka-topics --create --bootstrap-server kafka:9092 --partitions 2 --replication-factor 1 --topic car-engine
kafka-topics --create --bootstrap-server kafka:9092 --partitions 2 --replication-factor 1 --topic car-location
kafka-topics --create --bootstrap-server kafka:9092 --partitions 1 --replication-factor 1 --topic location-data --config "cleanup.policy=compact"
kafka-topics --create --bootstrap-server kafka:9092 --partitions 2 --replication-factor 1 --topic driver-notification

xxx

kafka-topics --delete --bootstrap-server kafka:9092 --topic car-speed
kafka-topics --delete --bootstrap-server kafka:9092 --topic car-engine
kafka-topics --delete --bootstrap-server kafka:9092 --topic car-location
kafka-topics --delete --bootstrap-server kafka:9092 --topic location-data
kafka-topics --delete --bootstrap-server kafka:9092 --topic driver-notification

 kafka-topics --bootstrap-server kafka:9092 --list

 kafka-console-consumer --bootstrap-server kafka:9092 --from-beginning --topic driver-notifications

docker compose up -d

sleep 5 # wait until broker is ready to accept requests

echo "Creating kafka topics..."
docker exec -it streams-app-tools-1 ./create-topics.sh

echo "Listing kafka topics..."
docker exec -it streams-app-tools-1 ./list-topics.sh

docker-compose down

 */
