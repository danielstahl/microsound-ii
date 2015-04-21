package music

import java.{lang => jl}
import net.soundmining.MusicPlayer
import net.soundmining.Utils._

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
  object AFTER_ACTION extends AddAction(new Integer(3))

  sealed case class Node(nodeId: Integer)
  object SOURCE extends Node(1004)
  object EFFECT extends Node(1005)
  object ROOM_EFFECT extends Node(1006)

  def setupNodes(player: MusicPlayer) = {
    val osc = Seq(
      player.makeGroupHead(0, SOURCE.nodeId),
      player.makeGroupTail(SOURCE.nodeId, EFFECT.nodeId),
      player.makeGroupTail(EFFECT.nodeId, ROOM_EFFECT.nodeId))
    player.sendBundle(absoluteTimeToMillis(0f), osc)
  }

  def buildFloat(value: Float): jl.Float = new jl.Float(value)
  def buildInteger(value: Int): jl.Integer = new jl.Integer(value)

  trait ArgumentBuilder {
    type SelfType <: InstrumentBuilder
    def self(): SelfType
  }

  object BusGenerator {
    var control = buildInteger(0)
    var audio = buildInteger(16)

    def reset() = {
      control = buildInteger(0)
      audio = buildInteger(16)
    }

    def nextControl(): jl.Integer = {
      val result = control
      control = buildInteger(control + 1)
      result
    }

    def nextAudio(): jl.Integer = {
      val result = audio
      audio = buildInteger(audio + 1)
      result
    }

    def currentControl: jl.Integer = control
    def currentAudio: jl.Integer = audio
  }

  trait InstrumentBuilder extends ArgumentBuilder {
    var instruments: Seq[InstrumentBuilder] = Seq(this)

    def addChild(instrument: InstrumentBuilder) = instruments = instruments :+ instrument
    def buildInstruments(): Seq[Seq[Object]] = instruments.reverseMap(_.build())

    def build(): Seq[Object]
  }

  abstract class AbstractInstrumentBuilder extends InstrumentBuilder {
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

  trait DurBuilder extends ArgumentBuilder {
    var dur: jl.Float = buildFloat(1f)

    def dur(value: Float): SelfType = {
      dur = buildFloat(value)
      self()
    }

    def buildDur(): Seq[Object] = Seq("dur", dur)
  }

  trait OutputBuilder extends ArgumentBuilder {
    var out: jl.Integer = buildInteger(0)

    def out(value: Int): SelfType = {
      out = buildInteger(value)
      self()
    }

    def buildOut(): Seq[Object] = Seq("out", out)
  }

  trait InputBuilder extends ArgumentBuilder {
    var in: jl.Integer = buildInteger(0)

    def in(value: Int): SelfType = {
      in = buildInteger(value)
      self()
    }

    def buildIn(): Seq[Object] = Seq("in", in)
  }

  case class ControlArgumentBuilder[ST <: InstrumentBuilder](me: ST, name: String) extends ArgumentBuilder {
    override type SelfType = ST
    override def self(): SelfType = me

    var bus: jl.Integer = buildInteger(0)

    def bus(value: Int): SelfType = {
      bus = buildInteger(value)
      self()
    }

    def control(controlInstrument: ControlInstrumentBuilder, controlReplaceInstruments: ControlReplaceInstrumentBuilder*): SelfType = {
      val outBus = BusGenerator.nextControl()
      controlInstrument.out(outBus)
      bus(outBus)
      me.addChild(controlInstrument)
      controlReplaceInstruments.foreach {
        controlReplaceInstrument =>
          controlReplaceInstrument.in(outBus)
          me.addChild(controlReplaceInstrument)
      }
      self()
    }

    def buildBus(): Seq[Object] = Seq(
      name, bus
    )
  }

  trait ControlInstrumentBuilder extends OutputBuilder with InstrumentBuilder with DurBuilder

  trait ControlReplaceInstrumentBuilder extends InputBuilder with InstrumentBuilder with DurBuilder

  class SineControlReplaceInstrumentBuilder extends AbstractInstrumentBuilder with ControlReplaceInstrumentBuilder {
    type SelfType = SineControlReplaceInstrumentBuilder
    def self(): SelfType = this

    var startFreq: jl.Float = _
    var endFreq: jl.Float = _

    def freq(start: Float, end: Float): SelfType = {
      startFreq = buildFloat(start)
      endFreq = buildFloat(end)
      self()
    }

    var startAmp: jl.Float = _
    var endAmp: jl.Float = _

    def amp(start: Float, end: Float): SelfType = {
      startAmp = buildFloat(start)
      endAmp = buildFloat(end)
      self()
    }

    val instrumentName: String = "sineControlReplace"

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        Seq(
          "startFreq", startFreq,
          "endFreq", endFreq,
          "startAmp", startAmp,
          "endAmp", endAmp
        )
  }

  object SineControlReplaceInstrumentBuilder {
    def sine(dur: Float, startFreq: Float, endFreq: Float, startAmp: Float, endAmp: Float, nodeId: Node = SOURCE): SineControlReplaceInstrumentBuilder = {
      new SineControlReplaceInstrumentBuilder()
        .dur(dur)
        .freq(startFreq, endFreq)
        .amp(startAmp, endAmp)
        .nodeId(nodeId)
    }
  }

  class LineControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
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

    def reverse: LineControlInstrumentBuilder =
      LineControlInstrumentBuilder.line(dur.floatValue(), endValue.floatValue(), startValue.floatValue())

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "startValue", startValue,
          "endValue", endValue
        )
  }

  object LineControlInstrumentBuilder {
    def line(dur: Float, start: Float, end: Float, nodeId: Node = SOURCE): LineControlInstrumentBuilder = {
      new LineControlInstrumentBuilder()
        .control(start, end)
        .dur(dur)
        .nodeId(nodeId)
    }
  }

  class ASRControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
    type SelfType = ASRControlInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "asrControl"

    var attackStart: jl.Float = buildFloat(1.0f)
    var sustainStart: jl.Float = buildFloat(1.0f)
    var decayStart: jl.Float = buildFloat(1.0f)
    var decayEnd: jl.Float = buildFloat(1.0f)

    def values(attackStartValue: Float, sustainStartValue: Float, decayStartValue: Float, decayEndValue: Float): SelfType = {
      attackStart = buildFloat(attackStartValue)
      sustainStart = buildFloat(sustainStartValue)
      decayStart = buildFloat(decayStartValue)
      decayEnd = buildFloat(decayEndValue)
      self()
    }

    var attackTime: jl.Float = buildFloat(1.0f)
    var sustainTime: jl.Float = buildFloat(1.0f)
    var decayTime: jl.Float = buildFloat(1.0f)

    def times(attackTimeValue: Float, sustainTimeValue: Float, decayTimeValue: Float): SelfType = {
      attackTime = buildFloat(attackTimeValue)
      sustainTime = buildFloat(sustainTimeValue)
      decayTime = buildFloat(decayTimeValue)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "attackStart", attackStart,
          "sustainStart", sustainStart,
          "decayStart", decayStart,
          "decayEnd", decayEnd,
          "attackTime", attackTime,
          "sustainTime", sustainTime,
          "decayTime", decayTime)
  }

  object ASRControlInstrumentBuilder {
    def asr(dur: Float, values: (Float, Float, Float, Float), times: (Float, Float, Float), nodeId: Node = SOURCE): ASRControlInstrumentBuilder = {
      new ASRControlInstrumentBuilder()
        .nodeId(nodeId)
        .values(values._1, values._2, values._3, values._4)
        .times(times._1, times._2, times._3)
        .dur(dur)
    }
  }

  object ARControlInstrumentBuilder {
    def ar(dur: Float, attackTime: Float, values: (Float, Float, Float), arType: (EnvCurve, EnvCurve) = (LINEAR, LINEAR), nodeId: Node = SOURCE): ARControlInstrumentBuilder = {
      new ARControlInstrumentBuilder()
        .nodeId(nodeId)
        .values(values._1, values._2, values._3)
        .types(arType._1, arType._2)
        .attackTime(attackTime)
        .dur(dur)
    }
  }

  class ARControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
    type SelfType = ARControlInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "arControl"

    var attackStart: jl.Float = buildFloat(1.0f)
    var releaseStart: jl.Float = buildFloat(1.0f)
    var releaseEnd: jl.Float = buildFloat(1.0f)

    def values(attackStartValue: Float, releaseStartValue: Float, releaseEndValue: Float): SelfType = {
      attackStart = buildFloat(attackStartValue)
      releaseStart = buildFloat(releaseStartValue)
      releaseEnd = buildFloat(releaseEndValue)
      self()
    }

    var attackTime: jl.Float = buildFloat(1.0f)

    def attackTime(attackTimeValue: Float): SelfType = {
      attackTime = buildFloat(attackTimeValue)
      self()
    }

    var attackType: EnvCurve = LINEAR
    var releaseType: EnvCurve = LINEAR

    def types(attackTypeValue: EnvCurve, releaseTypeValue: EnvCurve): SelfType = {
      attackType = attackTypeValue
      releaseType = releaseTypeValue
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "attackStart", attackStart,
          "releaseStart", releaseStart,
          "releaseEnd", releaseEnd,
          "attackTime", attackTime,
          "attackType", attackType.name,
          "releaseType", releaseType.name)
  }

  class FilterInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filt"

    val ampBus = ControlArgumentBuilder[FilterInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class FilterRejectInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterRejectInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtReject"

    val ampBus = ControlArgumentBuilder[FilterRejectInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterRejectInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterRejectInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class FilterReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = FilterReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtReplace"

    val ampBus = ControlArgumentBuilder[FilterReplaceInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterReplaceInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterReplaceInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class FilterRejectReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = FilterRejectReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "filtRejectReplace"

    val ampBus = ControlArgumentBuilder[FilterRejectReplaceInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterRejectReplaceInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterRejectReplaceInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class HighpassInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = HighpassInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "highPass"

    val freqBus = ControlArgumentBuilder[HighpassInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        freqBus.buildBus()
  }

  class HighpassReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = HighpassReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "highPassReplace"

    val freqBus = ControlArgumentBuilder[HighpassReplaceInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        freqBus.buildBus()
  }

  class LowpassInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = LowpassInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "lowPass"

    val freqBus = ControlArgumentBuilder[LowpassInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        freqBus.buildBus()
  }

  class LowpassReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = LowpassReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "lowPassReplace"

    val freqBus = ControlArgumentBuilder[LowpassReplaceInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        freqBus.buildBus()
  }

  abstract class CommonNoiseInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
    val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        ampBus.buildBus() ++
        buildDur()
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

  class PulseInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
    type SelfType = PulseInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "pulse"

    val ampBus = ControlArgumentBuilder[PulseInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[PulseInstrumentBuilder](this, "freqBus")
    val widthBus = ControlArgumentBuilder[PulseInstrumentBuilder](this, "widthBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        widthBus.buildBus()
  }

  abstract class CommonVolumeBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {

    val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        ampBus.buildBus()
  }

  class StereoVolumeBuilder extends CommonVolumeBuilder with OutputBuilder {
    type SelfType = StereoVolumeBuilder
    def self(): SelfType = this

    val instrumentName: String = "stereoVolume"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoVolumeBuilder extends CommonVolumeBuilder with OutputBuilder {
    type SelfType = MonoVolumeBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoVolume"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoVolumeReplaceBuilder extends CommonVolumeBuilder {
    type SelfType = MonoVolumeReplaceBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoVolumeReplace"
  }

  class PanInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = PanInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "pan"

    val panBus = ControlArgumentBuilder[PanInstrumentBuilder](self(), "panBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        buildOut() ++
        panBus.buildBus()
  }

  abstract class CommonMonoDelayInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    val delayBus = ControlArgumentBuilder[SelfType](self(), "delayBus")

    var maxDelay: jl.Float = buildFloat(0f)

    def maxDelay(value: Float): SelfType = {
      maxDelay = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        delayBus.buildBus() ++
        Seq("maxDelay", maxDelay)
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

  abstract class CommonMonoCombInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    val delayBus = ControlArgumentBuilder[SelfType](self(), "delayBus")

    val decayTimeBus = ControlArgumentBuilder[SelfType](self(), "decayTimeBus")

    var maxDelay: jl.Float = buildFloat(0f)

    def maxDelay(value: Float): SelfType = {
      maxDelay = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        delayBus.buildBus() ++
        decayTimeBus.buildBus() ++
        Seq("maxDelay", maxDelay)
  }

  class MonoCombInstrumentBuilder extends CommonMonoCombInstrumentBuilder with OutputBuilder {
    type SelfType = MonoCombInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoComb"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoCombReplaceInstrumentBuilder extends CommonMonoCombInstrumentBuilder {
    type SelfType = MonoCombReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoCombReplace"
  }

  abstract class CommonMonoAllpassInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    val delayBus = ControlArgumentBuilder[SelfType](self(), "delayBus")

    val decayTimeBus = ControlArgumentBuilder[SelfType](self(), "decayTimeBus")

    var maxDelay: jl.Float = buildFloat(0f)

    def maxDelay(value: Float): SelfType = {
      maxDelay = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        delayBus.buildBus() ++
        decayTimeBus.buildBus() ++
        Seq("maxDelay", maxDelay)
  }

  class MonoAllpassInstrumentBuilder extends CommonMonoCombInstrumentBuilder with OutputBuilder {
    type SelfType = MonoAllpassInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoAllpass"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoAllpassReplaceInstrumentBuilder extends CommonMonoCombInstrumentBuilder {
    type SelfType = MonoAllpassReplaceInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "monoAllpassReplace"
  }

  class GverbInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = GverbInstrumentBuilder
    def self(): SelfType = this

    val instrumentName: String = "gverb"

    var roomSize: jl.Float = buildFloat(0f)

    def roomSize(value: Float): SelfType = {
      roomSize = buildFloat(value)
      self()
    }

    var revTime: jl.Float = buildFloat(0f)

    def revTime(value: Float): SelfType = {
      revTime = buildFloat(value)
      self()
    }

    var damping: jl.Float = buildFloat(0f)

    def damping(value: Float): SelfType = {
      damping = buildFloat(value)
      self()
    }

    var inputBw: jl.Float = buildFloat(0f)

    def inputBw(value: Float): SelfType = {
      inputBw = buildFloat(value)
      self()
    }

    var spread: jl.Float = buildFloat(0f)

    def spread(value: Float): SelfType = {
      spread = buildFloat(value)
      self()
    }

    var earlyLevel: jl.Float = buildFloat(0f)

    def earlyLevel(value: Float): SelfType = {
      earlyLevel = buildFloat(value)
      self()
    }

    var tailLevel: jl.Float = buildFloat(0f)

    def tailLevel(value: Float): SelfType = {
      tailLevel = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "roomSize", roomSize,
          "revTime", revTime,
          "damping", damping,
          "inputBw", inputBw,
          "spread", spread,
          "earlyLevel", earlyLevel,
          "tailLevel", tailLevel)
  }

  def lineControlInstrument = new LineControlInstrumentBuilder
  def pulseInstrument = new PulseInstrumentBuilder
  def filterInstrument = new FilterInstrumentBuilder
  def filterRejectInstrument = new FilterRejectInstrumentBuilder
  def filterReplaceInstrument = new FilterReplaceInstrumentBuilder
  def filterRejectReplaceInstrument = new FilterRejectReplaceInstrumentBuilder
  def highPassInstrument = new HighpassInstrumentBuilder
  def highPassReplaceInstrument = new HighpassReplaceInstrumentBuilder
  def lowPassInstrument = new LowpassInstrumentBuilder
  def lowPassReplaceInstrument = new LowpassReplaceInstrumentBuilder
  def whiteNoiseInstrument = new WhiteNoiseInstrumentBuilder
  def pinkNoiseInstrument = new PinkNoiseInstrumentBuilder
  def panInstrument = new PanInstrumentBuilder
  def monoDelayInstrument = new MonoDelayInstrumentBuilder
  def monoDelayReplaceInstrument = new MonoDelayReplaceInstrumentBuilder
  def monoVolumeInstrument = new MonoVolumeBuilder
  def monoReplaceVolumeInstrument = new MonoVolumeReplaceBuilder
  def allpassInstrument = new MonoAllpassInstrumentBuilder
  def allpassReplaceInstrument = new MonoAllpassReplaceInstrumentBuilder
  def gverbInstrument = new GverbInstrumentBuilder
}
