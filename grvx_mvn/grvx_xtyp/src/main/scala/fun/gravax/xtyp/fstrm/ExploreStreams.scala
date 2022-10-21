package fun.gravax.xtyp.fstrm

private trait ExploreStreams

import cats.effect.{IO, IOApp}
import fs2.{Stream, text}

import fs2.io.file.{Files, Path}

object StreamingTempConverter extends IOApp.Simple {

	val converter: Stream[IO, Unit] = {
		def fahrenheitToCelsius(f: Double): Double =
			(f - 32.0) * (5.0/9.0)

		Files[IO].readAll(Path("tdat/inTempsFaren.txt"))
				.through(text.utf8.decode)
				.through(text.lines)
				.filter(s => !s.trim.isEmpty && !s.startsWith("//"))
				.map(line => fahrenheitToCelsius(line.toDouble).toString)
				.intersperse("\n")
				.through(text.utf8.encode)
				.through(Files[IO].writeAll(Path("tdat/outTempsCelsius.txt")))
	}

	def run: IO[Unit] = {
		val conv: Stream[IO, Unit] = converter
		val cc: Stream.CompileOps[IO, IO, Unit] = conv.compile
		val drained: IO[Unit] = cc.drain
		drained

	}
}
trait SomeStreamyBits {
	def fromGuide : Unit = {
		val x = Stream(1,0).repeat.take(6).toList
	}
}
