package fun.gravax.zpol

// import zio.stream.ZStream

private trait PollDataModelStuff


trait HasSizeConstraints {
	// What if allowed lengths are: 0, 2, 4...?

	// Can we test size constraints for equality?
}


trait Platform {
	def getAllClaimsInAnyOrder : GenStream[Claim]
	def getAllPositionsInAnyOrder : GenStream[Position]
	def getPositionForClaim(c : Claim) : Option[Position]

}

trait Profile {
	// For demographic information:  age, gender, location, ...
	// Date joined
	// Summary stats:  Claims published, p
}


trait Rule

trait Topic {
	// Topics are associated to claims (...with a strength?).
	// Are topics hierarchical?
	// How are they created, and associated to claims?
	// Can a position relate to topics other than the claim-topic?
	// Are topics like #HashTags?
	def getClaimsOrderedByDecreasingStrength : GenStream[Claim]
}

trait Claim {
	// Claims are immutable.  They are created by a platform, but may become orphaned.

	def getPlatform_opt : Option[Platform]
	// The author of a claim determines the rules which apply to it.
	// These rules determine:
	//  How complex the positions may be : One fragment, or several weighted fragments?
	// One reason per fragment, or several?
	//
	def getRules : GenStream[Rule]
	def getChoiceBinsInAnyOrder : GenStream[ChoiceBin]
}

trait ChoiceBin {
	def getSummary : String
	def getPositionsInAnyOrder : GenStream[Position]
}

trait PositionFrag {
	def getChoiceBin : String
	def getWeight : Float // TODO: Use rational?
	def getReasonClaimsOrderedByDecreasingImportance : GenStream[Claim]
	def getComment : String
}

trait Position {
	// Positions are mutable, and belong to a platform.
	// A platform may have only one position per claim.
	def getClaim : Claim
	def getPlatform : Platform
	// A position includes one or more fragments, which represent the
 	def getFragsInAnyOrder : GenStream[PositionFrag]
}

trait FullTextQueryIntf {
	// Things we can search by Text : Topic, Claim, Choice-Bin-Summary, Position-Frag-Comment
	// Should we have an "Entity" supertype for these?
	def findClaimsMatchingText(qryTxt : String)

	def dummy = {
		val x: List[List[String]] = List(List("Hey"))
		val y: Seq[String] = x.flatMap(innerL => innerL)
	}
}

