package fun.gravax.zpol

trait StreamUserStuff

trait HasSizeBounds extends HasSizeConstraints  {
	def getMinSize : Int = 0
	def getMaxSize_opt : Option[Int] = None
}
trait GenStream[X]
trait PureStream[X] extends GenStream[X] {
	// Intent is to offer a common denominator of fs2, Zstream, Akka Stream, JDK Stream, Scala-LazyList
	// Scala Stream is deprecated in 2.13, saying "Use LazyList (which is fully lazy) instead of Stream (which has a lazy tail only)"
	// But...LazyList is memoized!
}
trait FinStream[X] extends GenStream[X]

trait EmptyStream[X] extends FinStream[X]

trait SingletonStream[X] extends FinStream[X]

trait FinPureStream[X] extends PureStream[X] with FinStream[X]


trait StreamOps[S[_] <: GenStream[_]] {
	// These ops can only return streams - they can never produce any concrete value out.
	def isEmpty[X](inStrm : S[X]) : S[Boolean]
	def head[X](inStrm : S[X]) : S[X] 	// Constrained length
	def tail[X](inStrm : S[X]) : S[X] 	//
	def getFirstChunkSize : S[Int]
	def map[X,Y](inStrm : S[X]) : S[Y]
	def flatMap
	def filter
	def fold
	def append[X](inStrm : S[X], x : X) : S[X]
	def concat[X](inStrmA : S[X], inStrmB : S[X]) : S[X]
	def emptyStream[X] : S[X] with EmptyStream[X]
	def singletonPureStream[X](x : X) : S[X] with FinPureStream[X]
}

trait ConstrainedPureStream[X] {
}
trait EffectualStream[Eff, Err, X] extends GenStream[X]

class LLPureStream[X](val lzyLst : LazyList[X]) extends PureStream[X]


trait UsesStreams {
	type GStream[X] <: GenStream[X]
	type PStream[X] <: PureStream[X] with GStream[X]

	def getGStreamOps : StreamOps[GStream]
}

trait BallgameStreamTypeCtx extends UsesStreams

trait Player
trait PlayerRoster {
	def getPlayers : BallgameStreamTypeCtx#GStream[Player]
	def addPlayer(p : Player) : PlayerRoster
}
trait BattingLineup extends UsesStreams {
	def getName : String
	def getHitterStream : BallgameStreamTypeCtx#GStream[Player]
}


class ConcreteRoster[BSTC <: BallgameStreamTypeCtx](strmOps :StreamOps[BSTC#GStream])(plyrStrm: BSTC#GStream[Player]) extends PlayerRoster {
	override def getPlayers: BSTC#GStream[Player] = plyrStrm

	// To append p to our player stream, we need access to StreamOps.
	override def addPlayer(p: Player): PlayerRoster =  {
		val biggerStrm = strmOps.append(plyrStrm, p)
		val biggerRoster = new ConcreteRoster[BSTC](strmOps)(biggerStrm)
		biggerRoster
	}
}

trait RosterFactory extends BallgameStreamTypeCtx { self =>
	private lazy val myGSOps = getGStreamOps

	def mkRoster : PlayerRoster = {
		val emptyPlayerStream: GStream[Player] with EmptyStream[Player] = myGSOps.emptyStream[Player]
		new ConcreteRoster[self.type](myGSOps)(emptyPlayerStream)
	}
}

trait TradInheritStrm[X] extends GenStream[X] { self =>
	def appendToSelf(x: X): self.type // This sorta works IF we are willing to use explicit casts in the impls.
	type OutStrm <: TradInheritStrm[X]
	def appendOS(x: X) : OutStrm

}
trait TIStrmOps extends StreamOps[TradInheritStrm] {

}
class MoreOps() extends TIStrmOps {
	override def isEmpty[X](inStrm: TradInheritStrm[X]): TradInheritStrm[Boolean] = ???

	override def head[X](inStrm: TradInheritStrm[X]): TradInheritStrm[X] = ???

	override def tail[X](inStrm: TradInheritStrm[X]): TradInheritStrm[X] = ???

	override def getFirstChunkSize: TradInheritStrm[Int] = ???

	override def map[X, Y](inStrm: TradInheritStrm[X]): TradInheritStrm[Y] = ???

	override def flatMap: Unit = ???

	override def filter: Unit = ???

	override def fold: Unit = ???

	override def append[X](inStrm: TradInheritStrm[X], x: X): TradInheritStrm[X] = {
		inStrm.appendOS(x)
	}

	override def concat[X](inStrmA: TradInheritStrm[X], inStrmB: TradInheritStrm[X]): TradInheritStrm[X] = ???

	override def emptyStream[X]: TradInheritStrm[X] with EmptyStream[X] = EzEmptyStrm[X]

	override def singletonPureStream[X](x: X): TradInheritStrm[X] with FinPureStream[X] = ???
}
trait PureInheritStrm[X] extends PureStream[X] with TradInheritStrm[X]
abstract class LazyListStream[X](myLL : LazyList[X]) extends PureInheritStrm[X]  {
	def tail = ??? //  new LazyListStream[X](myLL.tail)
}
// We can make a nice general empty value, but how do we Append to it?

case class EzEmptyStrm[X]() extends PureInheritStrm[X] with EmptyStream[X] {
	// This does not work out too great.
	override type OutStrm = PureInheritStrm[X]

	override def appendToSelf(x: X) = ???

	override def appendOS(x: X) = ???
}

abstract class JavaStream[X]() extends PureInheritStrm[X] {
}



trait RosterWorkout {
	val myRostFact = new RosterFactory {
		override type GStream[X] = TradInheritStrm[X]
		override type PStream[X] = PureInheritStrm[X]
		lazy val myOps = new MoreOps
		override def getGStreamOps = myOps
	}

	def go : Unit = {
		val roster01a = myRostFact.mkRoster
		val wilmer = new Player {}
		val roster01b = roster01a.addPlayer(wilmer)
	}
}