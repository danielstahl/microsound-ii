package music

import java.lang.Float
import java.{lang => jl}

import music.Spectrum._

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

  sealed case class EnvCurve(name: String)
  object LINEAR extends EnvCurve("line")
  object SINE extends EnvCurve("sin")
  object EXPONENTIAL extends EnvCurve("exp")
  object WELCH extends EnvCurve("wel")
  object SQUARED extends EnvCurve("sqr")
  object CUBED extends EnvCurve("cub")

  sealed case class AddAction(action: Integer)
  object HEAD_ACTION extends AddAction(new Integer(0))
  object TAIL_ACTION extends AddAction(new Integer(1))
  object BEFORE_ACTION extends AddAction(new Integer(2))
  object AFTER_ACTION extends AddAction(new Integer(2))

  sealed case class Node(nodeId: Integer)
  object SOURCE extends Node(1001)
  object EFFECT extends Node(1002)


  def setupNodes(player: MusicPlayer) = {
    val osc = Seq(player.makeGroupHead(1, SOURCE.nodeId), player.makeGroupTail(SOURCE.nodeId, EFFECT.nodeId))
    player.sendBundle(absoluteTimeToMillis(0f), osc)
  }

  trait ArgumentBuilder {
    type SelfType
    def self(): SelfType
    def buildFloat(value: Float): jl.Float = new jl.Float(value)
  }

  abstract class InstrumentBuilder extends ArgumentBuilder {
    val instrumentName: String

    var addAction: AddAction = HEAD_ACTION

    def addAction(value: AddAction): SelfType = {
      addAction = value
      self()
    }

    var nodeId: Node = SOURCE

    def nodeId(value: Node): SelfType = {
      nodeId = value
      self()
    }

    def build(): Seq[Object] = {
      Seq(
        instrumentName,
        new Integer(-1), addAction.action, nodeId.nodeId
      )
    }
  }

  trait ASRBuilder extends ArgumentBuilder {
    val attackName: String
    val sustainName: String
    val decayStartName: String
    val decayEndName: String
    val attackTimeName: String
    val sustainTimeName: String
    val decayTimeName: String

    var attack: jl.Float = buildFloat(0f)

    def attack(value: Float): SelfType = {
      attack = buildFloat(value)
      self()
    }

    var sustain: jl.Float = buildFloat(0f)

    def sustain(value: Float): SelfType = {
      sustain = buildFloat(value)
      self()
    }

    var decayStart: jl.Float = buildFloat(0f)

    def decayStart(value: Float): SelfType = {
      decayStart = buildFloat(value)
      self()
    }

    var decayEnd: jl.Float = buildFloat(0f)

    def decayEnd(value: Float): SelfType = {
      decayEnd = buildFloat(value)
      self()
    }

    var attackTime: jl.Float = buildFloat(0f)

    def attackTime(value: Float): SelfType = {
      attackTime = buildFloat(value)
      self()
    }

    var sustainTime: jl.Float = buildFloat(0f)

    def sustainTime(value: Float): SelfType = {
      sustainTime = buildFloat(value)
      self()
    }

    var decayTime: jl.Float = buildFloat(0f)

    def decayTime(value: Float): SelfType = {
      decayTime = buildFloat(value)
      self()
    }

    def values(attackVal: Float, sustainVal: Float, decayStartVal: Float, decayEndVal: Float): SelfType = {
      attack = attackVal
      sustain = sustainVal
      decayStart = decayStartVal
      decayEnd = decayEndVal
      self()
    }

    def times(attackTimeVal: Float, sustainTimeVal: Float, decayTimeVal: Float): SelfType = {
      attackTime = attackTimeVal
      sustainTime = sustainTimeVal
      decayTime = decayTimeVal
      self()
    }

    def buildASR(): Seq[Object] = Seq(
      attackName, attack,
      sustainName, sustain,
      decayStartName, decayStart,
      decayEndName, decayEnd,
      attackTimeName, attackTime,
      sustainTimeName, sustainTime,
      decayTimeName, decayTime
    )
  }

  case class FreqASRBuilder[ST](me: ST) extends ASRBuilder {
    override type SelfType = ST
    override def self(): SelfType = me

    val attackName ="freqAttack"
    val sustainName = "freqSustain"
    val decayStartName ="freqDecayStart"
    val decayEndName ="freqDecayEnd"
    val attackTimeName ="freqAttackTime"
    val sustainTimeName ="freqSustainTime"
    val decayTimeName ="freqDecayTime"
  }

  case class WidthASRBuilder[ST](me: ST) extends ASRBuilder {
    override type SelfType = ST
    override def self(): SelfType = me

    val attackName ="widthAttack"
    val sustainName = "widthSustain"
    val decayStartName ="widthDecayStart"
    val decayEndName ="widthDecayEnd"
    val attackTimeName ="widthAttackTime"
    val sustainTimeName ="widthSustainTime"
    val decayTimeName ="widthDecayTime"
  }

  case class BwASRBuilder[ST](me: ST) extends ASRBuilder {
    override type SelfType = ST
    override def self(): SelfType = me

    val attackName ="bwAttack"
    val sustainName = "bwSustain"
    val decayStartName ="bwDecayStart"
    val decayEndName ="bwDecayEnd"
    val attackTimeName ="bwAttackTime"
    val sustainTimeName ="bwSustainTime"
    val decayTimeName ="bwDecayTime"
  }

  trait DurBuilder extends ArgumentBuilder {
    var dur: jl.Float = buildFloat(1f)

    def dur(value: Float): SelfType = {
      dur = buildFloat(value)
      self()
    }

    def buildDur(): Seq[Object] = Seq("dur", dur)
  }

  trait AmpBuilder extends ArgumentBuilder {

    var amp: jl.Float = buildFloat(1f)

    def amp(value: Float): SelfType = {
      amp = buildFloat(value)
      self()
    }

    def buildAmp(): Seq[Object] = Seq("amp", amp)
  }

  trait OutputBuilder extends ArgumentBuilder {
    var out: jl.Float = buildFloat(0f)

    def out(value: Float): SelfType = {
      out = buildFloat(value)
      self()
    }

    def buildOut(): Seq[Object] = Seq("out", out)
  }

  trait InputBuilder extends ArgumentBuilder {
    var in: jl.Float = buildFloat(0f)

    def in(value: Float): SelfType = {
      in = buildFloat(value)
      self()
    }

    def buildIn(): Seq[Object] = Seq("in", in)
  }

  trait CommonFilterInstrumentBuilder extends InstrumentBuilder {
    var startFreq: jl.Float = _
    var endFreq: jl.Float = _

    def freq(start: Float, end: Float): SelfType = {
      startFreq = buildFloat(start)
      endFreq = buildFloat(end)
      self()
    }

    var startBw: jl.Float = _
    var endBw: jl.Float = _

    def width(start: Float, end: Float): SelfType = {
      startBw = buildFloat(start)
      endBw = buildFloat(end)
      self()
    }

    def buildCommonFilter(): Seq[Object] = Seq(
      "startFreq", startFreq,
      "endFreq", endFreq,
      "startBw", startBw,
      "endBw", endBw
    )

  }

  class FilterInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with CommonFilterInstrumentBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filt"

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildAmp() ++
        buildDur() ++
        buildCommonFilter()
  }

  class FilterASRInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterASRInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtASR"

    val freqASRBuilder = FreqASRBuilder[FilterASRInstrumentBuilder](this)
    val bwASRBuilder = BwASRBuilder[FilterASRInstrumentBuilder](this)

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildAmp() ++
        buildDur() ++
        freqASRBuilder.buildASR() ++
        bwASRBuilder.buildASR()
  }

  class FilterReplaceInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with CommonFilterInstrumentBuilder with InputBuilder {
    type SelfType = FilterReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtReplace"

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildAmp() ++
        buildDur() ++
        buildCommonFilter()
  }

  class FilterReplaceASRInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with InputBuilder {
    type SelfType = FilterReplaceASRInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtReplaceASR"

    val freqASRBuilder = FreqASRBuilder[FilterReplaceASRInstrumentBuilder](this)
    val bwASRBuilder = BwASRBuilder[FilterReplaceASRInstrumentBuilder](this)

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildAmp() ++
        buildDur() ++
        freqASRBuilder.buildASR() ++
        bwASRBuilder.buildASR()
  }

  class PulseInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with OutputBuilder {
    type SelfType = PulseInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "pulse"

    var startFreq: jl.Float = _
    var endFreq: jl.Float = _

    def freq(start: Float, end: Float): PulseInstrumentBuilder = {
      startFreq = buildFloat(start)
      endFreq = buildFloat(end)
      this
    }

    var startWidth: jl.Float = _
    var endWidth: jl.Float = _

    def width(start: Float, end: Float): PulseInstrumentBuilder = {
      startWidth = buildFloat(start)
      endWidth = buildFloat(end)
      this
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildAmp() ++
        buildDur() ++
        Seq(
          "startFreq", startFreq,
          "endFreq", endFreq,
          "startWidth", startWidth,
          "endWidth", endWidth
        )
  }

  class PulseASRInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with OutputBuilder {
    type SelfType = PulseASRInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "pulseASR"

    val freqASRBuilder = FreqASRBuilder[PulseASRInstrumentBuilder](this)
    val widthASRBuilder = WidthASRBuilder[PulseASRInstrumentBuilder](this)

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildAmp() ++
        buildDur() ++
        freqASRBuilder.buildASR() ++
        widthASRBuilder.buildASR()
  }

  def pulseInstrument = new PulseInstrumentBuilder
  def pulseASRInstrument = new PulseASRInstrumentBuilder
  def filterInstrument = new FilterInstrumentBuilder
  def filterASRInstrument = new FilterASRInstrumentBuilder
  def filterReplaceInstrument = new FilterReplaceInstrumentBuilder
  def filterReplaceASRInstrument = new FilterReplaceASRInstrumentBuilder

  def absoluteTimeToMillis(time: Float): Long = (time * 1000).round.toLong

  def makeSeqWithIndex[T](seq: Seq[T]) = {
    0 until seq.size map {
      i => (i, seq(i))
    }
  }

  def main(args: Array[String]): Unit = {
    val player: MusicPlayer = MusicPlayer()

    println(s"fact is $phi")
    val overSpectrum = makeSpectrum(40, phi, 150)
    val underSpectrum = makeInvertedSpectrum(40, phi, 150)
    println(s"overSpectrum")
    makeSeqWithIndex(overSpectrum).foreach { case (i, v) => println(s"$i: $v")}
    println(s"underSpectrum")
    makeSeqWithIndex(underSpectrum).foreach { case (i, v) => println(s"$i: $v")}

    player.startPlay()

    val pulse = pulseInstrument
      .addAction(TAIL_ACTION)
      .out(16f)
      .dur(10f)
      .amp(0.1f)
      //.freq(0.8f, 0.4f)
      .freq(underSpectrum(1), underSpectrum(10))
      //.width(0.2f, 0.6f)
      .width(underSpectrum(99), underSpectrum(48))
      .build()

    val pulseASR = pulseASRInstrument
      .addAction(TAIL_ACTION)
      .out(16f)
      .dur(10f)
      .amp(0.1f)
      .freqASRBuilder.times(1f, 10f, 2f)
      .freqASRBuilder.values(underSpectrum(2), underSpectrum(10), underSpectrum(4), underSpectrum(2))
      .widthASRBuilder.times(1f, 10f, 2f)
      .widthASRBuilder.values(underSpectrum(65), underSpectrum(48), underSpectrum(24), underSpectrum(13))
      .build()

    val filterASR1 = filterASRInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      .freqASRBuilder.times(1f, 10f, 2f)
      .freqASRBuilder.values(overSpectrum(43), overSpectrum(44), overSpectrum(20), overSpectrum(19))
      .bwASRBuilder.times(1f, 10f, 2f)
      .bwASRBuilder.values(0.000001f, 0.000001f, 0.000001f, 0.000001f)
      .build()

    val filter1 = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      //.freq(2000f, 4000f)
      .freq(overSpectrum(44), overSpectrum(20))
      .width(0.000001f, 0.000001f)
      .build()

    val filter2 = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      .freq(overSpectrum(45), overSpectrum(25))
      .width(0.000001f, 0.000001f)
      .build()

    val filterASR2 = filterASRInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      .freqASRBuilder.times(1f, 10f, 2f)
      .freqASRBuilder.values(overSpectrum(46), overSpectrum(45), overSpectrum(25), overSpectrum(26))
      .bwASRBuilder.times(1f, 10f, 2f)
      .bwASRBuilder.values(0.000001f, 0.000001f, 0.000001f, 0.000001f)
      .build()

    val filter3 = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      .freq(overSpectrum(46), overSpectrum(52))
      .width(0.000001f, 0.000001f)
      .build()

    val filterASR3 = filterASRInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      .freqASRBuilder.times(1f, 10f, 2f)
      .freqASRBuilder.values(overSpectrum(47), overSpectrum(46), overSpectrum(52), overSpectrum(53))
      .bwASRBuilder.times(1f, 10f, 2f)
      .bwASRBuilder.values(0.000001f, 0.000001f, 0.000001f, 0.000001f)
      .build()

    val filter4 = filterInstrument
      .addAction(TAIL_ACTION)
      .in(16f)
      .out(0f)
      .dur(10f)
      .amp(0.1f)
      .freq(overSpectrum(47), overSpectrum(60))
      .width(0.000001f, 0.000001f)
      .build()



    setupNodes(player)
    //player.sendNew(absoluteTimeToMillis(0f), pulse, filter1, filter2, filter3, filter4)
    player.sendNew(absoluteTimeToMillis(0f), pulseASR, filterASR1, filter2, filterASR3, filter4)
    Thread.sleep(1000)
  }
}