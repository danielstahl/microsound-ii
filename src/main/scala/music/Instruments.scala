package music

import java.{lang => jl}
import Utils._
/**
 * Instruments
 */
object Instruments {
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

  case class BusArgumentBuilder[ST](me: ST, name: String) extends ArgumentBuilder {
    override type SelfType = ST
    override def self(): SelfType = me

    var bus: jl.Float = buildFloat(0f)

    def bus(value: Float): SelfType = {
      bus = buildFloat(value)
      self()
    }

    def buildBus(): Seq[Object] = Seq(
      name, bus
    )
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


  class LineControlInstrumentBuilder extends InstrumentBuilder with DurBuilder with OutputBuilder {
    type SelfType = LineControlInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "lineControl"

    var startValue: jl.Float = _
    var endValue: jl.Float = _

    def control(start: Float, end: Float): SelfType = {
      startValue = buildFloat(start)
      endValue = buildFloat(end)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "startValue", startValue,
          "endValue", endValue
        )
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

  class FilterRejectInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with CommonFilterInstrumentBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterRejectInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtReject"

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

  class FilterRejectReplaceInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with CommonFilterInstrumentBuilder with InputBuilder {
    type SelfType = FilterRejectReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtRejectReplace"

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

  abstract class CommonNoiseInstrumentBuilder extends InstrumentBuilder with DurBuilder with AmpBuilder with OutputBuilder {
    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildAmp() ++
        buildDur()
  }

  abstract class CommonNoiseInstrumentBuilder2 extends InstrumentBuilder with DurBuilder with OutputBuilder {
    val ampBus = BusArgumentBuilder[SelfType](self(), "ampBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        ampBus.buildBus() ++
        buildDur()
  }

  class PinkNoiseInstrumentBuilder2 extends CommonNoiseInstrumentBuilder2 {
    type SelfType = PinkNoiseInstrumentBuilder2
    def self(): SelfType = this

    val instrumentName: String = "pinkNoise2"
  }

  class PinkNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
    type SelfType = PinkNoiseInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "pinkNoise"
  }

  class WhiteNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
    type SelfType = WhiteNoiseInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "whiteNoise"
  }

  class WhiteNoiseInstrumentBuilder2 extends CommonNoiseInstrumentBuilder2 {
    type SelfType = WhiteNoiseInstrumentBuilder2
    def self(): SelfType = this

    val instrumentName: String = "whiteNoise2"
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


  abstract class CommonVolumeARBuilder extends InstrumentBuilder with DurBuilder with InputBuilder {
    var ampMin: jl.Float = buildFloat(0.00001f)
    var ampMax: jl.Float = buildFloat(1f)

    def amp(min: Float, max: Float): SelfType = {
      ampMin = buildFloat(min)
      ampMax = buildFloat(max)
      self()
    }

    var attackTime: jl.Float = buildFloat(0f)

    def attackTime(value: Float): SelfType = {
      attackTime = buildFloat(value)
      self()
    }

    var attackType: EnvCurve = LINEAR
    var releaseType: EnvCurve = LINEAR

    def arType(attack: EnvCurve = LINEAR, release: EnvCurve = LINEAR): SelfType = {
      attackType = attack
      releaseType = release
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        Seq(
          "ampMin", buildFloat(ampMin),
          "ampMax", buildFloat(ampMax),
          "attackTime", buildFloat(attackTime),
          "attackType", attackType.name,
          "releaseType", releaseType.name
        )
  }

  class StereoVolumeARBuilder extends CommonVolumeARBuilder with OutputBuilder {
    type SelfType = StereoVolumeARBuilder
    def self(): SelfType = this

    val instrumentName: String = "volumeAR"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }


  class MonoVolumeARBuilder extends CommonVolumeARBuilder with OutputBuilder {
    type SelfType = MonoVolumeARBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoVolumeAR"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoVolumeARReplaceBuilder extends CommonVolumeARBuilder {
    type SelfType = MonoVolumeARReplaceBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoVolumeARReplace"
  }

  abstract class CommonVolumeLineBuilder extends InstrumentBuilder with DurBuilder with InputBuilder {
    var startAmp: jl.Float = buildFloat(0.00001f)
    var endAmp: jl.Float = buildFloat(1f)

    def amp(start: Float, end: Float): SelfType = {
      startAmp = buildFloat(start)
      endAmp = buildFloat(end)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        Seq(
          "startAmp", buildFloat(startAmp),
          "endAmp", buildFloat(endAmp)
        )
  }



  class StereoVolumeLineBuilder extends CommonVolumeLineBuilder with OutputBuilder {
    type SelfType = StereoVolumeLineBuilder
    def self(): SelfType = this

    val instrumentName: String = "volumeLine"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }


  class MonoVolumeLineBuilder extends CommonVolumeLineBuilder with OutputBuilder {
    type SelfType = MonoVolumeLineBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoVolumeLine"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoVolumeLineReplaceBuilder extends CommonVolumeLineBuilder {
    type SelfType = MonoVolumeLineReplaceBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoVolumeLineReplace"
  }


  class PanInstrumentBuilder extends InstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = PanInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "pan"

    var startPan: jl.Float = buildFloat(0f)
    var endPan: jl.Float = buildFloat(0f)

    def pan(start: Float, end: Float): SelfType = {
      startPan = buildFloat(start)
      endPan = buildFloat(end)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        buildOut() ++
        Seq(
          "startPan", buildFloat(startPan),
          "endPan", buildFloat(endPan)
        )
  }

  abstract class CommonMonoDelayInstrumentBuilder extends InstrumentBuilder with DurBuilder with InputBuilder {

    var startDelay: jl.Float = buildFloat(0f)
    var endDelay: jl.Float = buildFloat(0f)

    def delay(start: Float, end: Float): SelfType = {
      startDelay = buildFloat(start)
      endDelay = buildFloat(end)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        Seq(
          "startDelay", buildFloat(startDelay),
          "endDelay", buildFloat(endDelay)
        )
  }

  class MonoDelayInstrumentBuilder extends CommonMonoDelayInstrumentBuilder with OutputBuilder {
    type SelfType = MonoDelayInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoDelay"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoDelayReplaceInstrumentBuilder extends CommonMonoDelayInstrumentBuilder {
    type SelfType = MonoDelayReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoDelayReplace"
  }

  def pulseInstrument = new PulseInstrumentBuilder
  def pulseASRInstrument = new PulseASRInstrumentBuilder
  def filterInstrument = new FilterInstrumentBuilder
  def filterRejectInstrument = new FilterRejectInstrumentBuilder
  def filterASRInstrument = new FilterASRInstrumentBuilder
  def filterReplaceInstrument = new FilterReplaceInstrumentBuilder
  def filterRejectReplaceInstrument = new FilterRejectReplaceInstrumentBuilder
  def filterReplaceASRInstrument = new FilterReplaceASRInstrumentBuilder
  def whiteNoiseInstrument = new WhiteNoiseInstrumentBuilder
  def pinkNoiseInstrument = new PinkNoiseInstrumentBuilder
  def stereoVolumeARInstrument = new StereoVolumeARBuilder
  def monoVolumeARInstrument = new MonoVolumeARBuilder
  def monoVolumeARReplaceInstrument = new MonoVolumeARReplaceBuilder
  def stereoVolumeLineInstrument = new StereoVolumeLineBuilder
  def monoVolumeLineInstrument = new MonoVolumeLineBuilder
  def monoVolumeLineReplaceInstrument = new MonoVolumeLineReplaceBuilder
  def panInstrument = new PanInstrumentBuilder
  def monoDelayInstrument = new MonoDelayInstrumentBuilder
  def monoDelayReplaceInstrument = new MonoDelayReplaceInstrumentBuilder
}
