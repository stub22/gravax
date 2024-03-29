package fun.gravax.dsktch.test

import fun.gravax.dsktch.quantile.{QuantileSketchReader, QuantileSketchWrapperMaker, QuantileSketchWriter, SketchDumperForBigDecimal}
import zio.stream.{UStream, ZSink, ZStream}
import zio.{UIO, ZIO, ZIOAppDefault, Random => ZRandom}

import java.io.IOException
import java.math.{MathContext, RoundingMode}
import java.util.Comparator
import scala.reflect.ClassTag

object RunDSktchZstrmDemo extends ZIOAppDefault {

	def run = myAppLogic

	val myAppLogic: ZIO[Any, IOException, Unit] = {
		val demoFeatures = new DSketchDemoFeatures {}
		for {
			_	<- ZIO.log("RunDSktchZstrmDemo BEGIN")
			sumTxt <- demoFeatures.summarizeOneStreamInSketch(32, 10 * 1000 * 1000, None)
			_ 	<- ZIO.log(s"Summarized stream as: ${sumTxt}")
		} yield ()
	}

}

trait DSketchDemoFeatures {
	private val myNumGenFeat = new NumGenFeatures {}
	private val myBigDecComp = new Comparator[BigDecimal] {
		override def compare(o1: BigDecimal, o2: BigDecimal): Int = o1.compare(o2)
	}

	def summarizeOneStreamInSketch(numSketchBins : Int, numSamples : Int, parStreamCnt_opt : Option[Int]) : UIO[String] = {
		val numGenUIO = myNumGenFeat.mkNumberGenUIO("3.5", "9.0", 8)
		val infiniteSampleStrm: UStream[BigDecimal] = ZStream.repeatZIO(numGenUIO)
		val finiteSampleStrm = infiniteSampleStrm.take(numSamples)
		val sketchReaderUIO = parStreamCnt_opt match {
			case Some(parStrmCnt) => summFiniteStrmPar(finiteSampleStrm, numSketchBins, myBigDecComp, parStrmCnt)
			case None => summarizeFiniteStream(finiteSampleStrm, numSketchBins, myBigDecComp)
		}
		val timedSketchUIO: UIO[(zio.Duration, QuantileSketchReader[BigDecimal])] = sketchReaderUIO.timed
		val summaryUIO: UIO[String] = timedSketchUIO.map(rsltPair => {
			val (dur, qsr) = rsltPair
			val dumper = new SketchDumperForBigDecimal(qsr)
			val deetTxt = dumper.getDetailedTxt(12, 13, true, false)
			deetTxt + s"\nK=${numSketchBins}, N=${numSamples}, parSteamCnt=${parStreamCnt_opt}, measuredDuration=${dur}"
		})
		summaryUIO
	}

	val myHSM = new QuantileSketchWrapperMaker {
		override protected def getFlg_UseHotWriter = false
		override protected def getFlg_UseBuffWriter = false
	}
	def summarizeFiniteStream[T <: Object : ClassTag](finStrm : UStream[T], numBins : Int, comp : Comparator[T]) :  UIO[QuantileSketchReader[T]] = {

		// Here we are *directly* making an *instance* that we treat as immutable (when hotWriter is false!), and hence reusable.
		// If we wanted to *not* reuse the instance (when hotWriter is true!), then we could wrap this creation in an effect.
		val emptyQSW = myHSM.mkEmptyWriteWrapper[T](numBins, comp)
		// Here we make a ZSink that uses our emptyQSW as a seed value.
		// This ZSink is reusable, but here we are only using it once.
		val accumSink = ZSink.foldLeft[T, QuantileSketchWriter[T]](emptyQSW)((prevQSW, nxtItem) => {
			val nxtQSW = prevQSW.addItem(nxtItem)
			nxtQSW
		})
		// Attach stream and sink to form a single effect
		val sketchMakerUIO: UIO[QuantileSketchWriter[T]] = finStrm.run(accumSink)
		// The reader access could be made part of a ZSink
		val sketchReaderUIO: UIO[QuantileSketchReader[T]] = sketchMakerUIO.map(endQSW => endQSW.getSketchReader)
		sketchReaderUIO
	}


	def summFiniteStrmPar[T <: Object : ClassTag](finStrm : UStream[T], numBins : Int, comp : Comparator[T], parGroups : Int) : UIO[QuantileSketchReader[T]] = {
		val emptyQSW_eff: UIO[QuantileSketchWriter[T]] = ZIO.succeed { myHSM.mkEmptyWriteWrapper[T](numBins, comp) }

		val strmWithIdx: UStream[(T, Long)] = finStrm.zipWithIndex
		val grouped: ZStream.GroupBy[Any, Nothing, Long, (T, Long)] = strmWithIdx.groupByKey(pair => pair._2 % parGroups)
		val readerUIO = emptyQSW_eff.flatMap(emptyQSW => {
			val accumSink = ZSink.foldLeft[T, QuantileSketchWriter[T]](emptyQSW)((prevQSW, nxtItem) => {
				val nxtQSW = prevQSW.addItem(nxtItem)
				nxtQSW
			})
			val mergedResultStrm: UStream[QuantileSketchWriter[T]] = grouped.apply((key, strm) => {
				val accumZIO: UIO[QuantileSketchWriter[T]] = strm.map(_._1).run(accumSink)
				val azs: UStream[QuantileSketchWriter[T]] = ZStream.fromZIO(accumZIO)
				azs
			})
			val dbgMRS = mergedResultStrm.debug("parAvgDumNum: partial result")
			// DANGER: Using the emptyQSW again
			val accumCombSink = ZSink.foldLeft[QuantileSketchWriter[T], QuantileSketchWriter[T]](emptyQSW)((prevCombo, nxtAcc) => {
				println(s"In accumComboSink-fold-lambda  prevComboClz=${prevCombo.getClass}, nxtAccClz=${nxtAcc.getClass}")
				prevCombo.mergeIfCompat(nxtAcc).get
			})
			val sketchMakerUIO: UIO[QuantileSketchWriter[T]] = dbgMRS.run(accumCombSink)
			val sketchReaderUIO: UIO[QuantileSketchReader[T]] = sketchMakerUIO.map(endQSW => endQSW.getSketchReader)
			sketchReaderUIO
		})
		readerUIO
	}

	def mkEmptyQSW_forBigDec(numBins : Int) : QuantileSketchWriter[BigDecimal] = {
		val hsm = new QuantileSketchWrapperMaker {}
		hsm.mkEmptyWriteWrapper[BigDecimal](numBins, myBigDecComp)
	}


}

trait NumGenFeatures {
	def mkNumberGenUIO(meanNumTxt : String, devNumTxt : String, precision : Int) : UIO[BigDecimal] = {
		val mean = BigDecimal(meanNumTxt)
		val dev = BigDecimal(devNumTxt)
		val mathCtx = new MathContext(precision, RoundingMode.HALF_UP)
		mkGaussianGenUIO(ZRandom.RandomLive, mathCtx, mean, dev)
	}

	def mkGaussianGenUIO(zrnd : ZRandom, mathCtx : MathContext,  mean : BigDecimal, stdDev : BigDecimal): UIO[BigDecimal] = {
		val stdNumEff: UIO[Double] = zrnd.nextGaussian
		stdNumEff.map(num => {
			// https://blogs.oracle.com/javamagazine/post/four-common-pitfalls-of-the-bigdecimal-class-and-how-to-avoid-them
			val stdBD = BigDecimal(num, mathCtx)
			val scaledAndShiftedBD = stdBD.*(stdDev).+(mean)
			scaledAndShiftedBD
		})
	}
}

/*
Output from Draft01b on 2023-03-29:

timestamp=2023-03-29T19:28:20.057Z level=INFO thread=#zio-fiber-4 message="RunDSktchZstrmDemo BEGIN" location=fun.gravax.dsktch.test.RunDSktchZstrmDemo.myAppLogic file=RunDSktchZstrmDemo.scala line=18
timestamp=2023-03-29T19:28:28.076Z level=INFO thread=#zio-fiber-4 message="Summarized stream as:
### ItemsSketch DATA DETAIL:
   BaseBuffer   : -2.2327227 -5.8277634 -5.3416266 -6.0822027 -5.6573476 -5.4960678 -4.7347369 -2.3504959 8.894823 -9.5132314 -6.5798126 -11.447816 -1.2846154 -13.311449 -8.1099416 -15.360588 -5.7469372 2.3761800 2.8561617 -4.5801577 -5.0285030 -8.6610898 1.0009098 -4.8260795 -8.5547507 -6.1470116 -10.476699 -3.7141563 -1.6690568 -1.1816667 -3.4588531 -1.9176231 4.6680066 -9.2454112 -3.5832164 -6.4161784 -4.6541427 -9.7608061 -3.7168116 3.8514388 -7.6151524 -3.6778078 -4.1125712 -7.9131786 -6.6907859 -8.0047652 4.2360988 -4.8550253 -9.4682044 -3.9321203 -4.2046545 -2.3463887 -9.1980010 -0.1017761 -2.2233924 -8.5454422 -3.3480413 -10.110448 -6.5788992 -8.1568501 -8.2000328 -6.1023606 -6.7705499 -12.977596
   Valid | Level
       T       0: -20.338357 -17.086875 -16.194353 -15.588516 -14.881634 -14.453112 -13.748007 -13.649420 -13.347471 -13.082237 -13.006646 -12.763004 -12.327354 -12.020793 -11.861992 -11.725043 -11.624200 -11.548751 -11.463179 -11.422410 -11.293080 -11.191434 -11.071709 -11.032248 -10.913830 -10.848486 -10.683210 -10.558422 -10.462468 -10.285368 -10.229549 -10.208833 -10.125333 -9.9786173 -9.8653195 -9.7586573 -9.5944024 -9.5565943 -9.5387167 -9.5053325 -9.4276311 -9.4259370 -9.3605916 -9.3152831 -9.2191105 -9.1230972 -9.1130708 -9.0263299 -8.9588647 -8.8535144 -8.7974987 -8.7382184 -8.5959809 -8.5608624 -8.5082620 -8.4259480 -8.4064834 -8.3873927 -8.3291253 -8.3042341 -8.2871614 -8.2353502 -8.2185164 -8.1872794 -8.1449109 -8.1272566 -8.0926407 -8.0722241 -8.0465949 -7.9955550 -7.9593094 -7.7004987 -7.6695480 -7.5870028 -7.5211477 -7.4390999 -7.3954404 -7.3130198 -7.2945147 -7.2634707 -7.2405899 -7.1610575 -7.0961367 -7.0471368 -7.0112182 -6.9349123 -6.8990736 -6.8718047 -6.7226290 -6.7131516 -6.6975885 -6.6437380 -6.6047729 -6.5951920 -6.5622623 -6.5217646 -6.4859958 -6.4780578 -6.4505840 -6.4333467 -6.4098399 -6.3861866 -6.3621246 -6.3381653 -6.3200364 -6.2781422 -6.2347198 -6.2191576 -6.1505522 -6.0749913 -6.0560260 -6.0436872 -6.0190218 -5.9988041 -5.9476819 -5.8918636 -5.8398832 -5.8272845 -5.7322881 -5.5868371 -5.5504590 -5.5276493 -5.5070633 -5.4821949 -5.4217867 -5.3755137 -5.2172780 -5.2009011 -5.1568952 -5.1096663 -5.0292330 -5.0087259 -4.9613482 -4.9368915 -4.9278699 -4.8917003 -4.8584578 -4.5963506 -4.5682759 -4.5410213 -4.5083072 -4.4816150 -4.4785075 -4.3431497 -4.3216714 -4.3067228 -4.2702487 -4.2499150 -4.2200624 -4.2046127 -4.1658217 -4.1313781 -4.0673044 -3.9817966 -3.9336018 -3.8881107 -3.8015117 -3.7609618 -3.6779018 -3.6151928 -3.5930120 -3.5503127 -3.4929969 -3.4309121 -3.4117919 -3.4105827 -3.4041886 -3.3737182 -3.3225356 -3.2211046 -3.1022858 -3.0179407 -2.9615816 -2.9236879 -2.8609122 -2.8499799 -2.7888030 -2.6663177 -2.5979526 -2.5618262 -2.4727170 -2.4094925 -2.3975240 -2.3321471 -2.2715289 -2.2166783 -2.2095855 -2.1851453 -2.1703162 -2.1144092 -2.0259977 -1.9212106 -1.8627224 -1.8377798 -1.6927376 -1.6464606 -1.5763982 -1.4571000 -1.4384662 -1.4120303 -1.4016428 -1.3107212 -1.2605542 -1.2401347 -1.1895458 -1.1008748 -1.0456459 -1.0334775 -0.9919415 -0.8147750 -0.7846902 -0.7741669 -0.7135056 -0.6799087 -0.5839569 -0.5286726 -0.5078826 -0.5036101 -0.4235981 -0.0500004 -0.0276998 0.0753790 0.1002721 0.1927264 0.2837781 0.2944588 0.4209480 0.5281492 0.5598544 0.5680696 0.7798216 0.8504019 0.9247365 1.0858936 1.1268063 1.2105594 1.3785286 1.4670387 1.5390661 1.6341732 1.6989165 1.7591719 1.9154043 1.9811250 2.1942801 2.5009546 2.6152753 2.6893332 3.1573831 3.3603655 3.6450346 4.2127641 4.4796064 5.254904 5.979706 8.234390
       F       1: null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null
       F       2: null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null
       F       3: null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null
       F       4: null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null
       T       5: -17.187855 -15.740492 -15.332207 -14.878750 -14.425960 -14.136250 -13.783602 -13.479590 -13.320078 -13.145564 -12.965859 -12.754049 -12.524858 -12.309076 -12.111453 -11.984757 -11.863747 -11.672043 -11.548747 -11.413822 -11.334623 -11.191943 -11.104521 -10.984322 -10.896000 -10.782408 -10.685285 -10.612871 -10.535004 -10.442444 -10.315855 -10.233640 -10.108975 -10.061577 -9.9866395 -9.9129682 -9.8032199 -9.7430671 -9.6610420 -9.5877856 -9.5138722 -9.4510564 -9.3857006 -9.3159918 -9.2486285 -9.1836273 -9.1218569 -9.0766564 -8.9906233 -8.9015519 -8.8335537 -8.7648934 -8.7119120 -8.6609787 -8.5915158 -8.5199525 -8.4799582 -8.3924290 -8.3551051 -8.3044858 -8.2630300 -8.2071075 -8.1099595 -8.0588831 -8.0086388 -7.9339483 -7.8852649 -7.8551108 -7.7957636 -7.7575449 -7.7132953 -7.6538606 -7.5863071 -7.5401314 -7.4884312 -7.4668774 -7.3990768 -7.3613764 -7.3129700 -7.2580434 -7.2083279 -7.1551515 -7.1126083 -7.0646878 -7.0121115 -6.9518272 -6.9164450 -6.8780518 -6.8251600 -6.7570156 -6.7174906 -6.6644765 -6.6343992 -6.5974523 -6.5296232 -6.4889014 -6.4465332 -6.4118049 -6.3545814 -6.3197925 -6.2556090 -6.2223032 -6.1800400 -6.1434142 -6.0692280 -6.0286540 -5.9937726 -5.9419709 -5.9139993 -5.8569774 -5.8072539 -5.7637206 -5.7354091 -5.6853709 -5.6502136 -5.5932945 -5.5663102 -5.4912313 -5.4527892 -5.4010546 -5.3570894 -5.3027347 -5.2758488 -5.2312070 -5.1669758 -5.1290242 -5.0825646 -5.0394395 -4.9938514 -4.9429745 -4.9178770 -4.8761235 -4.8307994 -4.7740423 -4.7294575 -4.6925537 -4.6555450 -4.6242705 -4.5870262 -4.5403040 -4.4740637 -4.4488735 -4.4047577 -4.3676671 -4.3095532 -4.2789686 -4.2259204 -4.1879907 -4.1543367 -4.1104144 -4.0723586 -4.0327576 -3.9803329 -3.9318461 -3.8916039 -3.8349783 -3.7712481 -3.7293416 -3.6891371 -3.6437637 -3.5949316 -3.5403260 -3.4984112 -3.4355682 -3.3995880 -3.3576530 -3.3147534 -3.2819288 -3.2049693 -3.1610404 -3.1159410 -3.0618407 -3.0131981 -2.9634596 -2.9294636 -2.8683837 -2.8372456 -2.7751775 -2.7189456 -2.6775998 -2.6281275 -2.5781631 -2.5451742 -2.4611862 -2.4273088 -2.3621993 -2.3190682 -2.2565847 -2.2016695 -2.1509342 -2.0894446 -1.9967471 -1.9654251 -1.9243445 -1.8685382 -1.8037221 -1.7416234 -1.6934638 -1.6330107 -1.5768297 -1.5363064 -1.4345315 -1.3829825 -1.3466693 -1.2940361 -1.2180900 -1.1403719 -1.0607477 -0.9905709 -0.8956429 -0.8542692 -0.7798420 -0.7150233 -0.6375156 -0.5594253 -0.4998704 -0.4107371 -0.3268031 -0.2527925 -0.1916276 -0.1109394 -0.0273744 0.0461979 0.1259541 0.2221119 0.3018271 0.4179798 0.5082633 0.5803587 0.6911766 0.8238730 0.9104350 1.0374061 1.1182698 1.2325779 1.3661748 1.5083194 1.6529278 1.7756910 1.9001466 2.0595555 2.1979962 2.3624905 2.5715781 2.7204426 2.8752039 3.1387509 3.4273800 3.6912852 3.9023964 4.2876585 4.6742044 5.249763 5.661775 6.950654 9.816333
       F       6: null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null null
       T       7: -20.162077 -17.043194 -15.821500 -15.066167 -14.778783 -14.404067 -13.970413 -13.685733 -13.418893 -13.115474 -12.884641 -12.686783 -12.510664 -12.298616 -12.195780 -11.989661 -11.839218 -11.700107 -11.595030 -11.498920 -11.430936 -11.277135 -11.156165 -11.051879 -10.945065 -10.843598 -10.740093 -10.636425 -10.554641 -10.455559 -10.396563 -10.284073 -10.203770 -10.130867 -10.022178 -9.9218197 -9.8455672 -9.7651000 -9.6681043 -9.6045827 -9.5186170 -9.4757060 -9.3741076 -9.3221499 -9.2200728 -9.1780593 -9.1033304 -9.0313446 -8.9578399 -8.9071115 -8.8392191 -8.7777875 -8.7433031 -8.6414933 -8.5804673 -8.5287129 -8.4694901 -8.4018659 -8.3440968 -8.2702872 -8.2196703 -8.1703930 -8.0897009 -8.0348324 -7.9823262 -7.9442838 -7.8752811 -7.8350601 -7.7801413 -7.7222543 -7.6701193 -7.6068776 -7.5646314 -7.5175868 -7.4672051 -7.4185846 -7.3731876 -7.3314281 -7.2835742 -7.2259446 -7.1654586 -7.1208358 -7.0727134 -7.0276224 -6.9654565 -6.9162387 -6.8734876 -6.8053749 -6.7700409 -6.7018610 -6.6697668 -6.6133238 -6.5791305 -6.5226182 -6.4713389 -6.4407078 -6.3818843 -6.3371741 -6.2889562 -6.2478007 -6.2126088 -6.1690096 -6.1359609 -6.0856235 -6.0355233 -6.0017432 -5.9631098 -5.9212162 -5.8627389 -5.8165399 -5.7839180 -5.7206311 -5.6761255 -5.6442337 -5.6018566 -5.5530490 -5.5059700 -5.4539168 -5.4302853 -5.3814317 -5.3424463 -5.2904590 -5.2465967 -5.2019633 -5.1402588 -5.0964333 -5.0558192 -5.0150938 -4.9767786 -4.9241659 -4.8975093 -4.8418000 -4.8098653 -4.7586030 -4.7159202 -4.6633062 -4.6160512 -4.5863833 -4.5352280 -4.4974130 -4.4563777 -4.4127438 -4.3640747 -4.3065324 -4.2718940 -4.2241627 -4.1809615 -4.1490608 -4.0996475 -4.0555880 -4.0194308 -3.9627806 -3.9223461 -3.8750343 -3.8322542 -3.7691612 -3.7344057 -3.6887008 -3.6372144 -3.6004406 -3.5394337 -3.5052749 -3.4665195 -3.4211036 -3.3781104 -3.3295391 -3.2887658 -3.2321031 -3.1761227 -3.1192404 -3.0839768 -3.0360375 -2.9784446 -2.9428576 -2.8793682 -2.8459286 -2.8003147 -2.7402611 -2.6872049 -2.6299148 -2.5950425 -2.5502045 -2.4992633 -2.4354026 -2.3874387 -2.3237856 -2.2798740 -2.2344064 -2.1884052 -2.1256424 -2.0459338 -1.9978587 -1.9359921 -1.8867446 -1.8254714 -1.7606700 -1.7104030 -1.6522445 -1.6042683 -1.5400161 -1.4734246 -1.4308886 -1.3747741 -1.3264360 -1.2604514 -1.1865736 -1.1112105 -1.0348757 -0.9794162 -0.9099766 -0.8502692 -0.7811482 -0.7289172 -0.6613010 -0.6052239 -0.5197278 -0.4448511 -0.3885854 -0.2815898 -0.2188818 -0.1583037 -0.0509841 0.0041044 0.1169041 0.1889343 0.2514505 0.3700998 0.4651335 0.5546133 0.6589102 0.7451892 0.8341182 0.9298188 1.0365385 1.1727445 1.2749886 1.4023687 1.5416505 1.6779712 1.8044154 1.9342516 2.1086275 2.2571271 2.3842620 2.5472538 2.7058612 2.9677009 3.1648729 3.4780216 3.7441548 4.0829188 4.5239197 4.8725734 5.212378 5.804437 7.335165
       T       8: -18.123244 -16.449231 -15.231606 -14.791902 -14.362778 -14.052818 -13.717402 -13.512810 -13.258927 -13.040148 -12.834303 -12.665067 -12.441698 -12.312840 -12.111988 -11.957767 -11.820977 -11.630894 -11.483305 -11.396513 -11.269122 -11.189582 -11.087234 -10.947360 -10.849389 -10.746231 -10.631547 -10.545880 -10.452390 -10.374469 -10.280526 -10.191680 -10.114280 -10.021338 -9.9452534 -9.8658568 -9.8009402 -9.7319782 -9.6548981 -9.5908100 -9.5046247 -9.4387770 -9.3563604 -9.2828745 -9.2117236 -9.1536504 -9.0815657 -9.0140528 -8.9518928 -8.8937805 -8.8270380 -8.7718222 -8.6993477 -8.6440559 -8.5691930 -8.5234283 -8.4527784 -8.4084212 -8.3442727 -8.2819927 -8.2256684 -8.1845881 -8.1231259 -8.0658793 -7.9970223 -7.9505352 -7.9053474 -7.8571778 -7.7904076 -7.7443664 -7.6858414 -7.6473442 -7.5848579 -7.5525332 -7.4836310 -7.4398594 -7.3716895 -7.3364701 -7.2841612 -7.2331261 -7.1962256 -7.1243165 -7.0826411 -7.0330360 -6.9900196 -6.9509800 -6.9101427 -6.8556014 -6.8031148 -6.7586894 -6.7183444 -6.6661420 -6.6191841 -6.5734001 -6.5249012 -6.4747491 -6.4321652 -6.3857024 -6.3275989 -6.2870361 -6.2239394 -6.1990090 -6.1481100 -6.1055368 -6.0567109 -6.0032112 -5.9493265 -5.9046420 -5.8605333 -5.8135716 -5.7781029 -5.7302750 -5.6887675 -5.6424657 -5.6030580 -5.5453705 -5.5115077 -5.4645100 -5.4191958 -5.3662305 -5.3332696 -5.2820728 -5.2291130 -5.1837522 -5.1368406 -5.0918844 -5.0419906 -5.0110334 -4.9707630 -4.9115002 -4.8729590 -4.8284095 -4.7840732 -4.7485547 -4.7141357 -4.6532162 -4.6163367 -4.5732860 -4.5119355 -4.4785187 -4.4302316 -4.3933584 -4.3397471 -4.2939040 -4.2453057 -4.2074713 -4.1563799 -4.0992443 -4.0590412 -4.0187436 -3.9749879 -3.9478676 -3.9044038 -3.8431711 -3.8030155 -3.7531609 -3.7014765 -3.6562526 -3.6059576 -3.5655210 -3.5201340 -3.4566283 -3.4211542 -3.3816830 -3.3284653 -3.2763604 -3.2430423 -3.1857167 -3.1402189 -3.0971435 -3.0502972 -3.0039845 -2.9482952 -2.9111664 -2.8504256 -2.7987486 -2.7556699 -2.7008082 -2.6743132 -2.6110938 -2.5532912 -2.5000045 -2.4511670 -2.3947656 -2.3437017 -2.2965018 -2.2483149 -2.2039840 -2.1374053 -2.0888037 -2.0360982 -1.9796914 -1.9265181 -1.8551651 -1.8062306 -1.7459208 -1.6971592 -1.6411337 -1.5839873 -1.5408452 -1.4691669 -1.4227065 -1.3474677 -1.2981993 -1.2303578 -1.1489252 -1.1088452 -1.0412578 -0.9617619 -0.8857068 -0.8233612 -0.7750602 -0.7113778 -0.6326283 -0.5506031 -0.4628835 -0.4126712 -0.3364047 -0.2409372 -0.1850342 -0.0936261 -0.0240777 0.0506083 0.1172596 0.2023843 0.2822539 0.3550581 0.4893394 0.5712264 0.6902212 0.7896330 0.9145084 1.0456114 1.1540367 1.2703990 1.3963554 1.5397209 1.6654288 1.7891145 1.9207579 2.0720551 2.2426677 2.3649412 2.4775398 2.6514297 2.8918867 3.0353116 3.2622165 3.5142799 3.8262906 4.1060650 4.5547893 4.9065102 5.580837 6.367023 8.126340
       T       9: -19.190852 -16.623025 -15.866315 -15.148783 -14.606842 -14.245018 -13.853454 -13.560324 -13.269264 -13.065863 -12.832895 -12.638241 -12.486463 -12.262874 -12.101281 -11.961931 -11.809662 -11.694551 -11.562733 -11.437076 -11.326614 -11.217712 -11.085045 -10.985244 -10.858990 -10.776359 -10.651247 -10.568687 -10.491877 -10.393521 -10.312074 -10.230578 -10.125494 -10.038249 -9.9618197 -9.8919973 -9.8191715 -9.7188184 -9.6502969 -9.5778712 -9.5080096 -9.4410959 -9.3694658 -9.2911121 -9.2278260 -9.1641691 -9.1116547 -9.0357442 -8.9504111 -8.8995711 -8.8332175 -8.7777991 -8.7157563 -8.6710053 -8.5873103 -8.5412127 -8.4732600 -8.3916426 -8.3390525 -8.3031044 -8.2258571 -8.1636925 -8.1232226 -8.0757113 -8.0105306 -7.9563587 -7.9021144 -7.8634688 -7.7773181 -7.7346633 -7.6839760 -7.6225123 -7.5819779 -7.5331337 -7.4863072 -7.4330724 -7.3742060 -7.3177858 -7.2693957 -7.2199894 -7.1683095 -7.1179011 -7.0672982 -7.0212656 -6.9729006 -6.9292903 -6.8888252 -6.8322861 -6.7872573 -6.7244109 -6.6912499 -6.6298807 -6.5878566 -6.5490804 -6.4934645 -6.4601261 -6.4021398 -6.3591496 -6.3131743 -6.2633631 -6.2202750 -6.1742162 -6.1278016 -6.0747334 -6.0348160 -5.9926907 -5.9546841 -5.8951361 -5.8660972 -5.8221404 -5.7733409 -5.7355206 -5.6910700 -5.6475834 -5.6059452 -5.5438992 -5.5049032 -5.4584480 -5.4114889 -5.3651844 -5.3233264 -5.2939601 -5.2305263 -5.1946840 -5.1502622 -5.1088290 -5.0612049 -5.0150584 -4.9715401 -4.9357038 -4.8846647 -4.8486195 -4.8024485 -4.7704341 -4.7140809 -4.6653873 -4.6226015 -4.5867484 -4.5438952 -4.4964891 -4.4559056 -4.4172257 -4.3757628 -4.3342988 -4.2822862 -4.2485213 -4.1831665 -4.1503931 -4.1118615 -4.0723672 -4.0071479 -3.9821130 -3.9295170 -3.8805533 -3.8393141 -3.7912630 -3.7584723 -3.7007084 -3.6591591 -3.6132255 -3.5675473 -3.5269686 -3.4757288 -3.4106044 -3.3704334 -3.3154682 -3.2536316 -3.2185764 -3.1607873 -3.1196056 -3.0806584 -3.0292939 -3.0007857 -2.9408051 -2.8846245 -2.8384286 -2.7937202 -2.7407933 -2.6815735 -2.6449288 -2.5888755 -2.5317768 -2.4850874 -2.4348038 -2.3870685 -2.3256720 -2.2756884 -2.2272728 -2.1726365 -2.1258048 -2.0692550 -2.0221398 -1.9382583 -1.8747704 -1.8333159 -1.7713402 -1.7307386 -1.6737874 -1.6077166 -1.5457078 -1.4910719 -1.4371644 -1.3700276 -1.2979225 -1.2523634 -1.1858040 -1.1174531 -1.0706867 -0.9926201 -0.9261038 -0.8648520 -0.7950106 -0.7031408 -0.6326581 -0.5697217 -0.4836942 -0.4137899 -0.3484175 -0.2814179 -0.2070297 -0.1200650 -0.0484592 0.0536418 0.1501290 0.2236945 0.3255597 0.4059283 0.4823316 0.5786370 0.6683953 0.7907994 0.8833203 1.0076656 1.0938550 1.2062925 1.3106533 1.4319004 1.5574491 1.7052498 1.8161343 2.0029131 2.1111237 2.2770558 2.4219783 2.6069967 2.8218460 2.9802339 3.2359765 3.4847937 3.6786982 3.9899002 4.3748950 4.8022830 5.364153 6.281250 7.280087
       T      10: -18.677307 -16.782697 -15.588801 -15.057278 -14.538906 -14.174214 -13.827284 -13.523491 -13.342170 -13.037076 -12.848015 -12.634773 -12.461546 -12.312334 -12.142675 -12.042587 -11.876592 -11.773086 -11.609117 -11.494193 -11.345244 -11.253973 -11.136387 -10.978556 -10.892467 -10.792897 -10.676644 -10.574434 -10.499083 -10.391476 -10.324877 -10.222748 -10.172676 -10.090897 -10.016046 -9.9281062 -9.8487662 -9.7646342 -9.6791698 -9.6006259 -9.5326633 -9.4704179 -9.3937379 -9.3328558 -9.2729071 -9.2091956 -9.1243239 -9.0703005 -8.9935493 -8.9229367 -8.8597023 -8.8039724 -8.7448429 -8.6716825 -8.6094661 -8.5484642 -8.4792871 -8.4255580 -8.3625536 -8.2997259 -8.2422019 -8.1958502 -8.1362632 -8.0883317 -8.0369923 -7.9742075 -7.9130074 -7.8676222 -7.8200892 -7.7506624 -7.7131406 -7.6481395 -7.6093519 -7.5489643 -7.5036655 -7.4547811 -7.4086621 -7.3505146 -7.3024012 -7.2471483 -7.1947156 -7.1462994 -7.0932661 -7.0468264 -6.9984987 -6.9439685 -6.8957290 -6.8571289 -6.8107784 -6.7471046 -6.7050502 -6.6615476 -6.6138344 -6.5625427 -6.5123551 -6.4609029 -6.4174865 -6.3667752 -6.3234992 -6.2867103 -6.2355100 -6.1933085 -6.1325853 -6.0946945 -6.0451930 -6.0121525 -5.9651045 -5.9238230 -5.8820811 -5.8362941 -5.7910197 -5.7381854 -5.6997222 -5.6550620 -5.6121177 -5.5640274 -5.5090986 -5.4723175 -5.4198525 -5.3784997 -5.3436843 -5.2952480 -5.2504646 -5.2116742 -5.1656782 -5.1164378 -5.0725011 -5.0203082 -4.9900524 -4.9459281 -4.9001818 -4.8620102 -4.8226678 -4.7719430 -4.7265850 -4.6778125 -4.6316621 -4.5972170 -4.5464765 -4.5045563 -4.4608171 -4.4036641 -4.3719402 -4.3255693 -4.2794239 -4.2318052 -4.1917094 -4.1424621 -4.0979878 -4.0525472 -4.0110842 -3.9735516 -3.9157203 -3.8772889 -3.8361070 -3.7786875 -3.7365076 -3.6988889 -3.6476580 -3.6109848 -3.5576507 -3.5128960 -3.4667852 -3.4189059 -3.3746422 -3.3255045 -3.2742459 -3.2269196 -3.1771631 -3.1315684 -3.0830829 -3.0425715 -2.9931763 -2.9321133 -2.8900709 -2.8454709 -2.7839433 -2.7438537 -2.6940746 -2.6317264 -2.5875528 -2.5442555 -2.4881060 -2.4472670 -2.3963272 -2.3371870 -2.2710258 -2.2221921 -2.1684933 -2.1170196 -2.0648264 -2.0052048 -1.9494916 -1.8964499 -1.8402003 -1.7897177 -1.7391305 -1.6657983 -1.6166123 -1.5833422 -1.5025300 -1.4257707 -1.3837662 -1.3144331 -1.2344517 -1.1722483 -1.1231431 -1.0782370 -0.9997485 -0.9204639 -0.8625218 -0.7806558 -0.7301032 -0.6711381 -0.5882392 -0.5287504 -0.4485299 -0.3773790 -0.3030642 -0.2137410 -0.1300802 -0.0540320 0.0479583 0.1255257 0.2225614 0.3036780 0.3812211 0.4873194 0.5960101 0.6648790 0.7607079 0.8675280 0.9722983 1.0748313 1.1781778 1.2795115 1.3995765 1.5425095 1.6962273 1.8469912 1.9865335 2.1176244 2.2479003 2.4488811 2.6257743 2.8592239 3.0027762 3.1781587 3.4538310 3.6703534 3.9428923 4.3434531 4.7618604 5.171118 6.025796 7.319347
### END DATA DETAIL

### ItemsSketch SUMMARY:
   K                            : 256
   N                            : 1,000,000
   BaseBufferCount              : 64
   CombinedBufferAllocatedCount : 3,328
   Total Levels                 : 11
   Valid Levels                 : 6
   Level Bit Pattern            : 11110100001
   Valid Samples                : 1,600
   Preamble Bytes               : 16
   Normalized Rank Error        : 0.717%
   Normalized Rank Error (PMF)  : 0.876%
   Min Value                    : -27.908162
   Max Value                    : 15.795879
### END SKETCH SUMMARY

quants: List(-27.908162, -10.985244, -9.1116547, -7.7346633, -6.5734001, -5.5090986, -4.4964891, -3.4211542, -2.2756884, -0.9204639, 1.0076656, 15.795879)
splits: List(-24.546313, -21.184463, -17.822614, -14.460765, -11.098915, -7.737066, -4.375217, -1.013368, 2.348482, 5.710331, 9.072180, 12.434030)
pmf: List(0.0, 0.0, 0.003842, 0.015113, 0.069349, 0.183857, 0.28362, 0.256723, 0.13614, 0.043543, 0.007749, 6.4E-5, 0.0)
pmfSum:0.9999999999999999" location=fun.gravax.dsktch.test.RunDSktchZstrmDemo.myAppLogic file=RunDSktchZstrmDemo.scala line=20


Testing with hielo pinned to "low power" mode, running at 2.3GHZ.
With copy ON for both ins and outs,
single threaded 1M samples, K=256 takes 6.9s
3M samples takes 19.1s
With K=32 time for 1M drops to 3.99s, 3M to 10.9s


 */