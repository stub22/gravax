package fun.gravax.distrib.gen

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
	def fetchOptional[FVT](itm : Item, fldNm : String)(implicit ev : zio.dynamodb.FromAttributeValue[FVT]) : Option[FVT] = {
		val fv_eith: Either[Nothing, Option[FVT]] = itm.getOptional[FVT](fldNm)
		fv_eith match {
			case Left(nothingHmm) => throw new Exception(s"Got exception fetching optional value at fldNm=${fldNm}. fv_eith=${fv_eith}")
			case Right(fVal_opt) => fVal_opt
		}
	}
}

trait ToItem {
	type FIType <: FromItem
	protected val myFromItem : FIType
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