
(
SynthDef(\pulseInstrument, {
	arg dur = 1, startPulseFreq, endPulseFreq,
	startFiltFreq, endFiltFreq,
	startFiltBw, endFiltBw,
	amp, startPan = 0.0, endPan = 0.0, attackTime = 0.5, attackType = \linear, decayType = \linear;
	var ampEnv, pulseFreqEnv, filtFreqEnv, filtBwEnv, pulse, filt;

	pulseFreqEnv = Line.kr(startPulseFreq, endPulseFreq, dur);
	filtFreqEnv = Line.kr(startFiltFreq, endFiltFreq, dur);
	filtBwEnv = Line.kr(startFiltBw * startFiltFreq, endFiltBw * endFiltFreq, dur);

	pulse = Pulse.ar(pulseFreqEnv);
	filt = BPF.ar(pulse, filtFreqEnv, filtBwEnv);
	filt = Balance.ar(filt, pulse);
	ampEnv = EnvGen.kr(Env([0.00001,amp,0.00001],[(attackTime * dur),
		((1 - attackTime) * dur)], [attackType, decayType]), doneAction:2);
	Out.ar(0, Pan2.ar(filt * ampEnv, Line.kr(startPan, endPan, dur)));

}).add;

SynthDef(\noiseInstrument, {
	arg dur = 1,
	startFiltFreq, endFiltFreq,
	startFiltBw, endFiltBw,
	amp, startPan = 0.0, endPan = 0.0, attackTime = 0.5, attackType = \linear, decayType = \linear;
	var ampEnv, pulseFreqEnv, filtFreqEnv, filtBwEnv, noise, filt;

	ampEnv = EnvGen.kr(Env([0.00001,amp,0.00001],[(attackTime * dur),
			((1 - attackTime) * dur)], attackType), doneAction:2);

	noise = PinkNoise.ar(ampEnv);

	filtFreqEnv = Line.kr(startFiltFreq, endFiltFreq, dur);
	filtBwEnv = Line.kr(startFiltBw * startFiltFreq, endFiltBw * endFiltFreq, dur);

	filt = BRF.ar(noise, filtFreqEnv, filtBwEnv);
	filt = Balance.ar(filt, noise);

	Out.ar(0, Pan2.ar(filt, Line.kr(startPan, endPan, dur)));

}).add;
)
