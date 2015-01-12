package music

import java.lang.Float
import java.{lang => jl}

import Spectrum._
import Instruments._
import Utils._

/**
 * Main class for the piece
 *
 * Data
overSpectrum
0: 20.0
1: 52.36068
2: 84.72136
3: 117.08205
4: 149.44272
5: 181.8034
6: 214.1641
7: 246.52475
8: 278.88544
9: 311.24612
10: 343.6068
11: 375.96747
12: 408.3282
13: 440.68884
14: 473.0495
15: 505.4102
16: 537.7709
17: 570.1316
18: 602.49225
19: 634.8529
20: 667.2136
21: 699.5743
22: 731.93494
23: 764.29565
24: 796.6564
25: 829.017
26: 861.3777
27: 893.73834
28: 926.099
29: 958.4597
30: 990.8204
31: 1023.1811
32: 1055.5417
33: 1087.9025
34: 1120.2632
35: 1152.6238
36: 1184.9845
37: 1217.3452
38: 1249.7058
39: 1282.0665
40: 1314.4272
41: 1346.7878
42: 1379.1486
43: 1411.5093
44: 1443.8699
45: 1476.2306
46: 1508.5913
47: 1540.952
48: 1573.3127
49: 1605.6733
50: 1638.034
51: 1670.3948
52: 1702.7554
53: 1735.1161
54: 1767.4767
55: 1799.8374
56: 1832.198
57: 1864.5587
58: 1896.9194
59: 1929.28
60: 1961.6407
61: 1994.0015
62: 2026.3622
63: 2058.723
64: 2091.0835
65: 2123.4443
66: 2155.805
67: 2188.1655
68: 2220.5264
69: 2252.887
70: 2285.2476
71: 2317.6084
72: 2349.969
73: 2382.3296
74: 2414.6904
75: 2447.051
76: 2479.4116
77: 2511.7722
78: 2544.133
79: 2576.4937
80: 2608.8545
81: 2641.2153
82: 2673.5757
83: 2705.9365
84: 2738.297
85: 2770.658
86: 2803.0186
87: 2835.379
88: 2867.7397
89: 2900.1003
90: 2932.4612
91: 2964.8218
92: 2997.1826
93: 3029.5432
94: 3061.904
95: 3094.2646
96: 3126.6255
97: 3158.9858
98: 3191.3467
99: 3223.7073
100: 3256.068
101: 3288.4287
102: 3320.7896
103: 3353.15
104: 3385.5107
105: 3417.8713
106: 3450.2322
107: 3482.5928
108: 3514.9534
109: 3547.3142
110: 3579.6748
111: 3612.0356
112: 3644.396
113: 3676.7568
114: 3709.1174
115: 3741.4783
116: 3773.8389
117: 3806.1997
118: 3838.56
119: 3870.921
120: 3903.2815
121: 3935.6423
122: 3968.003
123: 4000.3638
124: 4032.7244
125: 4065.0852
126: 4097.446
127: 4129.8066
128: 4162.167
129: 4194.5273
130: 4226.8887
131: 4259.249
132: 4291.61
133: 4323.97
134: 4356.331
135: 4388.692
136: 4421.0527
137: 4453.413
138: 4485.774
139: 4518.1343
140: 4550.495
141: 4582.856
142: 4615.217
143: 4647.577
144: 4679.938
145: 4712.299
146: 4744.659
147: 4777.02
148: 4809.381
149: 4841.741

underSpectrum
0: 20.0
1: 7.6393204
2: 4.7213597
3: 3.4164078
4: 2.6766107
5: 2.2001789
6: 1.8677268
7: 1.6225551
8: 1.4342806
9: 1.2851566
10: 1.1641213
11: 1.0639218
12: 0.97960424
13: 0.9076699
14: 0.8455775
15: 0.7914364
16: 0.7438112
17: 0.70159245
18: 0.66390896
19: 0.6300672
20: 0.59950817
21: 0.57177633
22: 0.5464966
23: 0.5233577
24: 0.50209856
25: 0.48249912
26: 0.46437237
27: 0.44755828
28: 0.43191925
29: 0.4173363
30: 0.40370587
31: 0.39093766
32: 0.37895232
33: 0.36768
34: 0.35705897
35: 0.3470343
36: 0.33755717
37: 0.32858387
38: 0.32007533
39: 0.3119963
40: 0.30431506
41: 0.29700297
42: 0.29003403
43: 0.28338462
44: 0.27703327
45: 0.27096036
46: 0.265148
47: 0.2595798
48: 0.25424063
49: 0.24911667
50: 0.24419516
51: 0.23946436
52: 0.23491336
53: 0.23053212
54: 0.22631134
55: 0.22224231
56: 0.21831702
57: 0.21452796
58: 0.21086821
59: 0.20733123
60: 0.20391093
61: 0.20060165
62: 0.19739808
63: 0.19429521
64: 0.1912884
65: 0.18837321
66: 0.18554555
67: 0.18280151
68: 0.18013747
69: 0.17754996
70: 0.17503573
71: 0.17259172
72: 0.17021501
73: 0.16790287
74: 0.1656527
75: 0.16346207
76: 0.1613286
77: 0.15925011
78: 0.15722449
79: 0.15524976
80: 0.15332401
81: 0.15144543
82: 0.14961237
83: 0.14782313
84: 0.14607619
85: 0.14437003
86: 0.14270331
87: 0.14107461
88: 0.13948266
89: 0.13792627
90: 0.13640419
91: 0.13491537
92: 0.13345867
93: 0.13203311
94: 0.13063768
95: 0.12927143
96: 0.12793346
97: 0.12662292
98: 0.12533894
99: 0.12408075
100: 0.12284755
101: 0.12163864
102: 0.12045328
103: 0.119290814
104: 0.11815056
105: 0.1170319
106: 0.11593422
107: 0.11485695
108: 0.11379952
109: 0.11276137
110: 0.111742
111: 0.110740885
112: 0.10975756
113: 0.10879153
114: 0.10784236
115: 0.10690962
116: 0.10599287
117: 0.105091706
118: 0.10420574
119: 0.10333458
120: 0.10247788
121: 0.10163525
122: 0.10080638
123: 0.099990904
124: 0.09918853
125: 0.098398924
126: 0.09762179
127: 0.09685684
128: 0.09610378
129: 0.09536235
130: 0.09463226
131: 0.09391327
132: 0.09320512
133: 0.09250757
134: 0.09182038
135: 0.09114333
136: 0.09047619
137: 0.08981875
138: 0.08917079
139: 0.08853211
140: 0.087902516
141: 0.08728182
142: 0.08666982
143: 0.08606635
144: 0.08547122
145: 0.08488426
146: 0.084305316
147: 0.083734214
148: 0.08317079
149: 0.082614906
 */
object Piece {

  import LineControlInstrumentBuilder._
  import ARControlInstrumentBuilder._
  import ASRControlInstrumentBuilder._
  import SineControlReplaceInstrumentBuilder._

  val overSpectrum = makeSpectrum(40, phi, 150)
  val underSpectrum = makeInvertedSpectrum(40, phi, 150)


  def makePulse(dur: Float): Seq[Seq[Object]] = {
    val pulse = pulseInstrument
    .addAction(TAIL_ACTION)
    .out(16)
    .dur(dur)
    .freqBus.control(line(dur, underSpectrum(149), underSpectrum(98)))
    .widthBus.control(line(dur, underSpectrum(48), underSpectrum(34)))
    .ampBus.control(line(dur, 0.1f, 0.5f))
    .buildInstruments()

    val pulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(16)
      .out(0)
      .panBus.control(line(dur, 0.6f, 0.3f))
      .buildInstruments()

    pulse ++ pulsePan
  }

  def makePulseFilter1(dur: Float, soundBus: Int): Seq[Seq[Object]] = {
    val pulseFilter = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16)
      .out(soundBus)
      .dur(dur)
      .ampBus.control(line(dur, 0.004f, 0.0007f))
      .freqBus.control(line(dur, overSpectrum(5), overSpectrum(6)))
      .bwBus.control(line(dur, 0.0000001f, 0.00000001f))
      .buildInstruments()

    val pulseFilterDelay = monoDelayReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .delayBus.control(ar(dur, 0.3f, (0.1f, 0.15f, 0.04f)), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f))
      .maxDelay(0.2f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.9f), sine(dur, 0.6f, 0.5f, 0.05f, 0.06f))
      .delayBus.control(line(dur, 0.07f, 0.09f), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f))
      .maxDelay(0.1f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f))
      .delayBus.control(line(dur, 0.01f, 0.009f))
      .maxDelay(0.01f)
      .buildInstruments()

    val filterPulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(soundBus)
      .out(0)
      .panBus.control(line(dur, -0.8f, -1f))
      .buildInstruments()

    pulseFilter ++ pulseFilterDelay ++ combFilter ++ allpassFilter ++ filterPulsePan
  }

  def makePulseFilter2(dur: Float, soundBus: Int): Seq[Seq[Object]] = {
    val pulseFilter = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16)
      .out(soundBus)
      .dur(dur)
      .ampBus.control(line(dur, 0.003f, 0.001f))
      .freqBus.control(line(dur, overSpectrum(22), overSpectrum(10)))
      .bwBus.control(line(dur, 0.0000001f, 0.00000001f))
      .buildInstruments()

    val pulseFilterDelay = monoDelayReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .delayBus.control(ar(dur, 0.3f, (0.01f, 0.1f, 0.02f)), sine(dur, underSpectrum(48), underSpectrum(48), 0.001f, 0.001f))
      .maxDelay(0.2f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.2f, 1.3f), sine(dur, underSpectrum(40), underSpectrum(48), 0.05f, 0.06f))
      .delayBus.control(line(dur, 0.1f, 0.2f), sine(dur, 0.7f, 0.3f, 0.01f, 0.008f))
      .maxDelay(0.3f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 1.3f, 1.4f))
      .delayBus.control(line(dur, 0.15f, 0.2f), sine(dur, 0.09f, 0.1f, 0.05f, 0.09f))
      .maxDelay(0.2f)
      .buildInstruments()

    val filterPulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(soundBus)
      .out(0)
      .panBus.control(line(dur, -0.7f, -0.8f))
      .buildInstruments()

    pulseFilter ++ pulseFilterDelay ++ combFilter ++ allpassFilter ++ filterPulsePan
  }

  def makePulseFilter3(dur: Float, soundBus: Int): Seq[Seq[Object]] = {
    val pulseFilter = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16)
      .out(soundBus)
      .dur(dur)
      .ampBus.control(line(dur, 0.006f, 0.003f))
      .freqBus.control(line(dur, overSpectrum(47), overSpectrum(57)))
      .bwBus.control(line(dur, 0.0000001f, 0.00000001f))
      .buildInstruments()

    val pulseFilterDelay = monoDelayReplaceInstrument
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .delayBus.control(ar(dur, 0.3f, (0.11f, 0.15f, 0.12f)), sine(dur, 0.1f, 0.2f, 0.01f, 0.01f))
      .maxDelay(0.15f)
      .buildInstruments()

    val combFilter = new MonoCombReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.3f), sine(dur, 0.2f, 0.1f, 0.03f, 0.02f))
      .delayBus.control(line(dur, 0.12f, 0.1f), sine(dur, 0.07f, 0.04f, 0.008f, 0.01f))
      .maxDelay(0.15f)
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .in(soundBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 0.6f, 0.8f))
      .delayBus.control(line(dur, 0.1f, 0.15f), sine(dur, 0.02f, 0.04f, 0.01f, 0.02f))
      .maxDelay(0.15f)
      .buildInstruments()

    val filterPulsePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(soundBus)
      .out(0)
      .panBus.control(line(dur, -0.6f, -0.5f))
      .buildInstruments()

    pulseFilter ++ pulseFilterDelay ++ combFilter ++ allpassFilter ++ filterPulsePan
  }


  def makeNoise(dur: Float, audioBus: Int): Seq[Seq[Object]] = {
      val whiteNoise = whiteNoiseInstrument
        .addAction(TAIL_ACTION)
        .dur(dur)
        .out(audioBus)
        .ampBus.control(asr(dur, (0.0f, 0.1f, 0.15f, 0.0f), (0.01f, 10f, 0.2f)))
        .buildInstruments()

    whiteNoise
  }

  def makeNoiseFilter1(dur: Float, audioInBus: Int, audioOutBus: Int): Seq[Seq[Object]] = {
    val rejectFilter = filterRejectInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.01f, 0.001f))
      .freqBus.control(line(dur, overSpectrum(5), overSpectrum(6)))
      .buildInstruments()

    val filter = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.3f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(line(dur, overSpectrum(5), overSpectrum(6)))
      .buildInstruments()

    val noisePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioOutBus)
      .out(0)
      .panBus.control(line(dur, 0.6f, 0.5f))
      .buildInstruments()

    rejectFilter ++ filter ++ noisePan
  }

  def makeNoiseFilter2(dur: Float, audioInBus: Int, audioOutBus: Int): Seq[Seq[Object]] = {
    val rejectFilter = filterRejectInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.5f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.01f, 0.001f))
      .freqBus.control(line(dur, overSpectrum(22), overSpectrum(10)))
      .buildInstruments()

    val filter = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.5f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(line(dur, overSpectrum(22), overSpectrum(10)))
      .buildInstruments()

    val noisePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioOutBus)
      .out(0)
      .panBus.control(line(dur, 0.7f, 0.8f))
      .buildInstruments()

    rejectFilter ++ filter ++ noisePan
  }


  def makeNoiseFilter3(dur: Float, audioInBus: Int, audioOutBus: Int): Seq[Seq[Object]] = {
    val rejectFilter = filterRejectInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.001f, 0.0001f))
      .freqBus.control(line(dur, overSpectrum(47), overSpectrum(57)))
      .buildInstruments()

    val filter = filterInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioInBus)
      .out(audioOutBus)
      .ampBus.control(ar(dur, 0.7f, (0.0f, 0.07f, 0.0f)))
      .bwBus.control(line(dur, 0.000001f, 0.0000001f))
      .freqBus.control(line(dur, overSpectrum(47), overSpectrum(57)))
      .buildInstruments()

    val noisePan = panInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(audioOutBus)
      .out(0)
      .panBus.control(line(dur, 0.9f, 0.3f))
      .buildInstruments()

    rejectFilter ++ filter ++ noisePan
  }

  /**
   * The first movement.
   * The idea is to have two main layers
   * 1. A slow pulse with a slowly evolving freq (about 0.5/second)
   * and width (0.5 - 0.1)
   * 2. A noise that crossfade from white to pink
   * with a slowly moving filter.
   */
  def firstMovement(): Unit = {
    BusGenerator.reset()
    val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    val dur = overSpectrum(1)

    setupNodes(player)


    val messages =
      makePulse(dur) ++
      makePulseFilter1(dur, 17) ++
      makePulseFilter2(dur, 18) ++
      makePulseFilter3(dur, 19) ++
      makeNoise(dur, 20) ++
      makeNoiseFilter1(dur, 20, 21) ++
      makeNoiseFilter2(dur, 20, 22) ++
      makeNoiseFilter3(dur, 20, 23)
    player.sendNew(absoluteTimeToMillis(0f), messages.toSeq: _*)


    Thread.sleep(1000)
  }


  def makeSeqWithIndex[T](seq: Seq[T]) = {
    0 until seq.size map {
      i => (i, seq(i))
    }
  }

  def main(args: Array[String]): Unit = {
    //println(s"overSpectrum")
    //makeSeqWithIndex(overSpectrum).foreach { case (i, v) => println(s"$i: $v")}
    //println(s"underSpectrum")
    //makeSeqWithIndex(underSpectrum).foreach { case (i, v) => println(s"$i: $v")}

    firstMovement()
  }
}
