package fun.gravax.xtyp.study.algo

trait  AlgoMedian {

	/*
	Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.
	The overall run time complexity should be O(log (m+n)).
	 */
		def findMedianSortedArrays(numsA: Array[Int], numsB: Array[Int]): Double = {

			val lenA = numsA.length
			val lenB = numsB.length
			val (bigArr, smallArr) = if (lenA >= lenB) (numsA, numsB) else (numsB, numsA)
			// We need to move our cursors in the bigArr first.

			val (bigLen, smallLen) = (bigArr.length, smallArr.length)
			// For odd Len, the mid will be exact.  For even Len, we will get the element just right of mid.
			val (bigMidPos, smallMidPos) = (bigLen / 2, smallLen / 2)
			val (bigMidNum, smallMidNum) = (bigArr(bigMidPos), smallArr(smallMidPos))
			???


		}

		// Find the median which is known to be between bigLeft and bigRight
		//  @tailrec
		def findMed(bigArr : Array[Int], bigLeftBound : Int, bigRightBound : Int,
					smallArr : Array[Int], smallLeftBound : Int, smallRightBound : Int) : Double = {

			assert(bigLeftBound <= bigRightBound)
			assert(smallLeftBound <= smallRightBound)
			val (bigLen, smallLen) = (bigArr.length, smallArr.length)
			assert(smallRightBound < smallLen)
			assert(bigRightBound < bigLen)
			val comboLen = bigLen + smallLen
			// For an odd length, these will be the same position.  (7 gives 3, 3 which is the 4th element)
			// For an even length, these will be adjoining positions. (8 gives 3, 4 - the 4th and 5th elements)
			val comboMedPosLeft = (comboLen - 1) / 2
			val comboMedPosRight = comboLen / 2
			val (exclBigLeft, exclBigRight) = (bigLeftBound, bigLen - bigRightBound - 1)
			val (exclSmallLeft, exclSmallRight) = (smallLeftBound, smallLen - smallRightBound - 1)
			val totalExclLeft = exclBigLeft + exclSmallLeft
			val totalExclRight = exclBigRight + exclSmallRight
			val leftGap = comboMedPosLeft - totalExclLeft
			val rightGap = comboLen - (comboMedPosRight + totalExclRight)
			assert (leftGap >= 0)
			assert (rightGap >= 0)

			val (bigLeftNum, bigRightNum) = (bigArr(bigLeftBound), bigArr(bigRightBound))
			val (smallLeftNum, smallRightNum) = (smallArr(smallLeftBound), smallArr(smallRightBound))
			// We know there are at least totalExclLeft entries that are <= highLeftNum
			val highLeftNum = Math.max(bigLeftNum, smallLeftNum)
			// We know there are at least totalExclRight entries that are >= lowRightNum
			val lowRightNum = Math.min(bigRightNum, smallRightNum)

			val hmm = if (leftGap == 0) {
				// We have excluded the required number of items on the left, so neither leftBound will increase further.
			}
			???
		}
}
