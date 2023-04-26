package fun.gravax.zdynamo

import zio.dynamodb.{AttributeValue, DynamoDBError, Item}

trait GenItemStuff

trait FromItem {
	def fetchOrThrow[FVT](itm : Item, fldNm : String)(implicit ev : zio.dynamodb.FromAttributeValue[FVT]) : FVT = {
		val fv_eith: Either[DynamoDBError, FVT] = itm.get[FVT](fldNm)
		fv_eith match {
			case Left(ddbErr) => throw ddbErr
			case Right(fVal) => fVal
		}
	}
}

trait ToItem {
	protected val myFromItem : FromItem
	// Will throw if any parts of the sort-key are not found in the partItm.
	def fillSortKey(partItm : Item, fn_sortKey : String, fns_sortKeyParts : Seq[String], separator : String) : Item = {
		// Assumes that sort-key part-values are all Strings.
		val pim: Map[String, AttributeValue] = partItm.map
		if (pim.contains(fn_sortKey)) partItm else {
			val sortKeyVals: Seq[String] = fns_sortKeyParts.map(skpn => myFromItem.fetchOrThrow[String](partItm, skpn))
			val compoundSortKey = sortKeyVals.mkString(separator)
			val attrVal_sortKey = AttributeValue(compoundSortKey)
			val fullPim = pim + (fn_sortKey -> attrVal_sortKey)
			val fullItem = Item(fullPim)
			fullItem
		}
	}
}