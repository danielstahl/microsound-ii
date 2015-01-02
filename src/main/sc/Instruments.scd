
(
Synth("lineControl", [dur: 1, startValue: 0.4, endValue: 0.1, out: 0], addAction:\addToTail);
Synth("pinkNoise2", [dur: 1, ampBus: 0, out: 0], addAction:\addToTail);
)

(
Synth("lineControl", [dur: 1, startValue: 0.4, endValue: 0.1, out: 0], addAction:\addToTail);
Synth("whiteNoise2", [dur: 1, ampBus: 0, out: 0], addAction:\addToTail);
)

(
Synth("lineControl", [dur: 10, startValue: 0, endValue: 0.5, out: 0], addAction:\addToTail);
Synth("lineControl", [dur: 10, startValue: 40, endValue: 40, out: 1], addAction:\addToTail);
Synth("lineControl", [dur: 10, startValue: 0.1, endValue: 0.9, out: 2], addAction:\addToTail);
Synth("pulse2", [dur: 10, amp: 1, ampBus: 0, freqBus: 1, widthBus: 2, out: 0], addAction:\addToTail);
)

/* Moduler instruments */
(

SynthDef(\pinkNoise, {
	arg dur = 1, amp = 0.5, out = 0;
	var noise, ampEnv;
	noise = PinkNoise.ar(amp);
	ampEnv = Line.kr(amp, amp, dur, doneAction:2);
	Out.ar(out, noise * ampEnv);
}).add;

SynthDef(\lineControl, {
	arg dur = 1, startValue = 1, endValue = 1, out = 0;
	var lineEnv;
	lineEnv = Line.kr(startValue, endValue, dur, doneAction:2);
	Out.kr(out, lineEnv);
}).add;

SynthDef(\asrControl, {
	arg dur, out = 0,
	attackStart, sustainStart, decayStart, decayEnd,
	attackTime, sustainTime, decayTime;
	var asrEnv;
	asrEnv = Env(
		levels:[attackStart, sustainStart, decayStart, decayEnd],
		times: [attackTime, sustainTime, decayTime],
		curve: \sin);
	asrEnv.duration = dur;
	Out.kr(out, EnvGen.kr(asrEnv, doneAction:2));
}).add;

SynthDef(\arControl, {
	arg dur, out = 0,
	attackStart, releaseStart, releaseEnd,
	attackTime, attackType = \linear, releaseType = \linear;
	var arEnv;
	arEnv = Env(
		levels:[attackStart, releaseStart, releaseEnd],
		times: [attackTime, 1 - attackTime],
		curve: [attackType, releaseType]);
	arEnv.duration = dur;
	Out.kr(out, EnvGen.kr(arEnv, doneAction:2));
}).add;

SynthDef(\pinkNoise2, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	noise = PinkNoise.ar();
	amp = In.kr(ampBus);
	Out.ar(out, noise * amp);
}).add;


SynthDef(\whiteNoise, {
	arg dur = 1, amp = 0.5, out = 0;
	var noise, ampEnv;
	noise = WhiteNoise.ar(amp);
	ampEnv = Line.kr(amp, amp, dur, doneAction:2);
	Out.ar(out, noise * ampEnv);
}).add;

SynthDef(\whiteNoise2, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	noise = WhiteNoise.ar();
	amp = In.kr(ampBus);
	Out.ar(out, noise * amp);
}).add;

SynthDef(\pulse, {
	arg dur = 1, amp = 1, startFreq, endFreq, startWidth, endWidth, out = 0;
	var pulse, freqEnv, widthEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	widthEnv = Line.kr(startWidth, endWidth, dur);
	pulse = Pulse.ar(freqEnv, widthEnv);
	Out.ar(out, pulse * amp);
}).add;

SynthDef(\pulse2, {
	arg dur = 1, ampBus = 0, freqBus = 1, widthBus = 2, out = 0;
	var pulse, amp, freq, width;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	width = In.kr(widthBus);
	pulse = Pulse.ar(freq, width);
	Out.ar(out, pulse * amp);
}).add;

SynthDef(\pulseASR, {
	arg dur = 1, amp, out = 0,
	freqAttack, freqSustain, freqDecayStart, freqDecayEnd,
	freqAttackTime, freqSustainTime, freqDecayTime,
	widthAttack, widthSustain, widthDecayStart, widthDecayEnd,
	widthAttackTime, widthSustainTime, widthDecayTime;
	var pulse, freqEnv, widthEnv;
	freqEnv = Env(
		levels:[freqAttack, freqSustain, freqDecayStart, freqDecayEnd],
		times: [freqAttackTime, freqSustainTime, freqDecayTime],
		curve: \sin);
	freqEnv.duration = dur;
	widthEnv = Env(
		levels: [widthAttack, widthSustain, widthDecayStart, widthDecayEnd],
		times: [widthAttackTime, widthSustainTime, widthDecayTime],
		curve: \sin);
	widthEnv.duration = dur;
	pulse = Pulse.ar(
		EnvGen.kr(freqEnv, doneAction: 2),
		EnvGen.kr(widthEnv));
	Out.ar(out, pulse * amp);

}).add;

SynthDef(\filt, {
	arg dur = 1, amp = 1, startFreq, endFreq, startBw, endBw, in = 0, out = 0;
	var filt, freqEnv, bwEnv, input;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	bwEnv = Line.kr(startBw * startFreq, endBw * endFreq, dur);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freqEnv, bwEnv);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;


SynthDef(\filt2, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0, out = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtReject, {
	arg dur = 1, amp = 1, startFreq, endFreq, startBw, endBw, in = 0, out = 0;
	var filt, freqEnv, bwEnv, input;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	bwEnv = Line.kr(startBw * startFreq, endBw * endFreq, dur);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freqEnv, bwEnv);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtReject2, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0, out = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;

SynthDef(\filtASR, {
	arg dur = 1, amp = 1, in = 0, out = 0,
	freqAttack, freqSustain, freqDecayStart, freqDecayEnd,
	freqAttackTime, freqSustainTime, freqDecayTime,
	bwAttack, bwSustain, bwDecayStart, bwDecayEnd,
	bwAttackTime, bwSustainTime, bwDecayTime;
	var filt, freqEnv, bwEnv, input;

	freqEnv = Env(
		levels:[freqAttack, freqSustain, freqDecayStart, freqDecayEnd],
		times: [freqAttackTime, freqSustainTime, freqDecayTime],
		curve: \sin);
	freqEnv.duration = dur;
	bwEnv = Env(
		levels: [bwAttack, bwSustain, bwDecayStart, bwDecayEnd],
		times: [bwAttackTime, bwSustainTime, bwDecayTime],
		curve: \sin);
	bwEnv.duration = dur;

	input = In.ar(in, 1);
	filt = BPF.ar(input,
		EnvGen.kr(freqEnv, doneAction: 2),
		EnvGen.kr(bwEnv));
	filt = Balance.ar(filt, input);
	Out.ar(out, filt * amp);
}).add;


SynthDef(\filtReplace, {
	arg dur = 1, amp = 1, startFreq, endFreq, startBw, endBw, in = 0;
	var filt, freqEnv, bwEnv, input;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	bwEnv = Line.kr(startBw * startFreq, endBw * endFreq, dur);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freqEnv, bwEnv);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\filtReplace2, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\filtRejectReplace, {
	arg dur = 1, amp = 1, startFreq, endFreq, startBw, endBw, in = 0;
	var filt, freqEnv, bwEnv, input;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	bwEnv = Line.kr(startBw * startFreq, endBw * endFreq, dur);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freqEnv, bwEnv);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\filtRejectReplace2, {
	arg dur = 1, ampBus = 0, freqBus = 1, bwBus = 2, in = 0;
	var filt, amp, freq, bw, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	bw = In.kr(bwBus);
	input = In.ar(in, 1);
	filt = BRF.ar(input, freq, bw * freq);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;


SynthDef(\filtReplaceASR, {
	arg dur = 1, amp = 1, in = 0,
	freqAttack, freqSustain, freqDecayStart, freqDecayEnd,
	freqAttackTime, freqSustainTime, freqDecayTime,
	bwAttack, bwSustain, bwDecayStart, bwDecayEnd,
	bwAttackTime, bwSustainTime, bwDecayTime;
	var filt, freqEnv, bwEnv, input;

	freqEnv = Env(
		levels:[freqAttack, freqSustain, freqDecayStart, freqDecayEnd],
		times: [freqAttackTime, freqSustainTime, freqDecayTime],
		curve: \sin);
	freqEnv.duration = dur;

	bwEnv = Env(
		levels: [bwAttack, bwSustain, bwDecayStart, bwDecayEnd],
		times: [bwAttackTime, bwSustainTime, bwDecayTime],
		curve: \sin);
	bwEnv.duration = dur;

	input = In.ar(in, 1);
	filt = BPF.ar(input,
		EnvGen.kr(freqEnv, doneAction: 2),
		EnvGen.kr(bwEnv));
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;


SynthDef(\limit, {
	arg level = 1, dur = 0.01, in = 0, out = 0;
	var input;
	input = In.ar(in, 1);
	Out.ar(out, Limiter.ar(input, level, dur));
}).add;

SynthDef(\limitReplace, {
	arg level = 1, dur = 0.01, in = 0;
	var input;
	input = In.ar(in, 1);
	ReplaceOut.ar(in, Limiter.ar(input, level, dur));
}).add;

SynthDef(\pan, {
	arg dur = 1, startPan, endPan, in = 0, out = 0;
	var panEnv, input;
	panEnv = Line.kr(startPan, endPan, dur, doneAction:2);
	input = In.ar(in, 1);
	Out.ar(out, LinPan2.ar(input, panEnv));
}).add;

SynthDef(\pan2, {
	arg dur = 1, panBus = 0, in = 0, out = 0;
	var pan, input;
	pan = In.kr(panBus);
	input = In.ar(in, 1);
	Out.ar(out, LinPan2.ar(input, pan));
}).add;


SynthDef(\volumeAR, {
	arg dur = 1, ampMin = 0.00001, ampMax = 1,
	attackTime = 0.5, attackType = \linear, releaseType = \linear,
	in = 0, out = 0;
	var ampEnv, input;
	ampEnv =
	EnvGen.kr(
		Env([ampMin,ampMax,ampMin],
			[(attackTime * dur),((1 - attackTime) * dur)],
			[attackType, releaseType]), doneAction:2);
	input = In.ar(in, 2);
	Out.ar(out, input * ampEnv);
}).add;

SynthDef(\stereoVolume2, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 2);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolumeAR, {
	arg dur = 1, ampMin = 0.00001, ampMax = 1,
	attackTime = 0.5, attackType = \linear, releaseType = \linear,
	in = 0, out = 0;
	var ampEnv, input;
	ampEnv =
	EnvGen.kr(
		Env([ampMin,ampMax,ampMin],
			[(attackTime * dur),((1 - attackTime) * dur)],
			[attackType, releaseType]), doneAction:2);
	input = In.ar(in, 1);
	Out.ar(out, input * ampEnv);
}).add;

SynthDef(\monoVolume2, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolumeReplace2, {
	arg dur = 1, ampBus = 0, in = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * amp);
}).add;

SynthDef(\monoVolumeARReplace, {
	arg dur = 1, ampMin = 0.00001, ampMax = 1,
	attackTime = 0.5, attackType = \linear, releaseType = \linear,
	in = 0;
	var ampEnv, input;
	ampEnv =
	EnvGen.kr(
		Env([ampMin,ampMax,ampMin],
			[(attackTime * dur),((1 - attackTime) * dur)],
			[attackType, releaseType]), doneAction:2);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * ampEnv);
}).add;

SynthDef(\volumeLine, {
	arg dur = 1, startAmp = 1, endAmp = 1, in = 0, out = 0;
	var ampEnv, input;
	ampEnv = Line.kr(startAmp, endAmp, dur, doneAction:2);

	input = In.ar(in, 2);
	Out.ar(out, input * ampEnv);
}).add;

SynthDef(\monoVolumeLine, {
	arg dur = 1, startAmp = 1, endAmp = 1, in = 0, out = 0;
	var ampEnv, input;
	ampEnv = Line.kr(startAmp, endAmp, dur, doneAction:2);

	input = In.ar(in, 1);
	Out.ar(out, input * ampEnv);
}).add;

SynthDef(\monoVolumeLineReplace, {
	arg dur = 1, startAmp = 1, endAmp = 1, in = 0;
	var ampEnv, input;
	ampEnv = Line.kr(startAmp, endAmp, dur, doneAction:2);

	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * ampEnv);
}).add;

SynthDef(\volumeASR, {
	arg dur = 1, in = 0, out = 0,
	ampAttack = 0.00001, ampSustain, ampDecayStart, ampDecayEnd = 0.00001,
	ampAttackTime, ampSustainTime, ampDecayTime;

	var ampEnv, input;

	ampEnv = Env(
		levels:[ampAttack, ampSustain, ampDecayStart, ampDecayEnd],
		times: [ampAttackTime, ampSustainTime, ampDecayTime],
		curve: \sin);
	ampEnv.duration = dur;

	input = In.ar(in, 2);
	Out.ar(out, input * EnvGen.kr(ampEnv, doneAction:2));
}).add;

SynthDef(\monoVolumeASR, {
	arg dur = 1, in = 0, out = 0,
	ampAttack = 0.00001, ampSustain, ampDecayStart, ampDecayEnd = 0.00001,
	ampAttackTime, ampSustainTime, ampDecayTime;

	var ampEnv, input;

	ampEnv = Env(
		levels:[ampAttack, ampSustain, ampDecayStart, ampDecayEnd],
		times: [ampAttackTime, ampSustainTime, ampDecayTime],
		curve: \sin);
	ampEnv.duration = dur;

	input = In.ar(in, 1);
	Out.ar(out, input * EnvGen.kr(ampEnv, doneAction:2));
}).add;

SynthDef(\monoVolumeASR, {
	arg dur = 1, in = 0,
	ampAttack = 0.00001, ampSustain, ampDecayStart, ampDecayEnd = 0.00001,
	ampAttackTime, ampSustainTime, ampDecayTime;

	var ampEnv, input;

	ampEnv = Env(
		levels:[ampAttack, ampSustain, ampDecayStart, ampDecayEnd],
		times: [ampAttackTime, ampSustainTime, ampDecayTime],
		curve: \sin);
	ampEnv.duration = dur;

	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * EnvGen.kr(ampEnv, doneAction:2));
}).add;

SynthDef(\monoDelay, {
	arg dur = 1, in = 0, out = 0,
	startDelay, endDelay;
	var delayEnv, input, maxDelay;

	maxDelay = max(startDelay, endDelay);
	delayEnv = Line.kr(startDelay, endDelay, dur, doneAction:2);

	input = In.ar(in, 1);

	Out.ar(out, DelayN.ar(input, maxDelay, delayEnv));
}).add;

SynthDef(\monoDelay2, {
	arg dur = 1, in = 0, out = 0, delayBus = 0, maxDelay = 0.1;
	var delay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	input = In.ar(in, 1);
	Out.ar(out, DelayN.ar(input, maxDelay, delay));
}).add;

SynthDef(\monoDelayReplace, {
	arg dur = 1, in = 0,
	startDelay, endDelay;
	var delayEnv, input, maxDelay;

	maxDelay = max(startDelay, endDelay);
	delayEnv = Line.kr(startDelay, endDelay, dur, doneAction:2);

	input = In.ar(in, 1);

	ReplaceOut.ar(in, DelayN.ar(input, maxDelay, delayEnv));
}).add;

SynthDef(\monoDelayReplace2, {
	arg dur = 1, in = 0, delayBus = 0, maxDelay = 0.1;
	var delay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, DelayN.ar(input, maxDelay, delay));
}).add;

SynthDef(\monoComb, {
	arg dur = 1, in = 0, out = 0,
	startDelay, endDelay, startDecayTime, endDecayTime;
	var delayEnv, decayEnv, input, maxDelay;

	maxDelay = max(startDelay, endDelay);
	delayEnv = Line.kr(startDelay, endDelay, dur, doneAction:2);
	decayEnv = Line.kr(startDecayTime, endDecayTime, dur);

	input = In.ar(in, 1);

	Out.ar(out, CombL.ar(input, maxDelay, delayEnv, decayEnv));
}).add;

SynthDef(\monoComb2, {
	arg dur = 1, in = 0, out = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);
	Out.ar(out, CombL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\monoCombReplace, {
	arg dur = 1, in = 0,
	startDelay, endDelay, startDecayTime, endDecayTime;
	var delayEnv, decayEnv, input, maxDelay;

	maxDelay = max(startDelay, endDelay);
	delayEnv = Line.kr(startDelay, endDelay, dur, doneAction:2);
	decayEnv = Line.kr(startDecayTime, endDecayTime, dur);

	input = In.ar(in, 1);

	ReplaceOut.ar(in, CombL.ar(input, maxDelay, delayEnv, decayEnv));
}).add;

SynthDef(\monoCombReplace2, {
	arg dur = 1, in = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);

	ReplaceOut.ar(in, CombL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\reverb, {
	arg in = 0, out = 0, predelay = 0.048,
	combMinTime = 0.01, combDelayTime = 0.1, combDecay = 5,
	numAllpass = 6, allpassMinTime = 0.001, allpassDelayTime = 0.05, allpassDecaytime = 1;
	var input, numc, temp, numComb;

	input= In.ar(in, 2); //get two channels of input

	numComb = 4; // number of comb delays

	// reverb predelay time :
	temp = DelayL.ar(input, predelay, predelay);

	// Comb filter
	temp = Mix.fill(numComb,{
		CombL.ar(temp, combDelayTime, Rand(combMinTime, combDelayTime), combDecay);
	});

	// chain of allpass delays on each of two channels:
	numAllpass.do({
		temp = AllpassL.ar(temp, allpassDelayTime,
			[Rand(allpassMinTime, allpassDelayTime), Rand(allpassMinTime, allpassDelayTime)],
			allpassDecaytime)
	});

	// Output the reverb
	Out.ar(out, temp);
}).add;
)

/*
http://community.dur.ac.uk/nick.collins/teaching/supercollider/sctutorial/6.4 Effects 1.html
*/
(
Synth("pulse", [dur: 10, amp: 0.01, startFreq: 0.8, endFreq: 0.4, startWidth: 0.2, endWidth: 0.22, out: 16],
	addAction:\addToTail);
Synth("filt", [dur: 10, amp: 0.3, startFreq: 200, endFreq: 300, startBw: 0.000001, endBw: 0.000001, in: 16, out: 17],
	addAction:\addToTail);
Synth("limit", [level: 0.4, in: 17, out: 18], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: -1, endPan: 1, in: 18, out: 20], addAction:\addToTail);
Synth("reverb", [in: 20, out: 22, predelay: 0.048, combMinTime: 0.01, combDelayTime: 0.1, combDecay: 5, numAllpass: 6, allpassMinTime: 0.001, allpassDelayTime: 0.05, allpassDecaytime: 1, mix: 0.04], addAction:\addToTail);
Synth("volumeLine", [dur: 10, in: 20, startAmp: 1, endAmp: 1],addAction:\addToTail);
Synth("volumeLine", [dur: 10, in: 22, startAmp: 0.03, endAmp: 0.03],addAction:\addToTail);
)


/*
"http://ecmc.rochester.edu/ecmc/docs/supercollider/scbook/Ch21_Interface_Investigations/ixi SC tutorial/ixi_SC_tutorial_12.html"
*/

(
Synth("pulse", [dur: 10, amp: 0.01, startFreq: 0.8, endFreq: 0.4, startWidth: 0.2, endWidth: 0.22, out: 16], addAction:\addToTail);
Synth("filt", [dur: 10, amp: 0.3, startFreq: 200, endFreq: 300, startBw: 0.000001, endBw: 0.000001, in: 16, out: 17], addAction:\addToTail);
Synth("limit", [level: 0.4, in: 17, out: 18], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: -1, endPan: 1, in: 18, out: 20], addAction:\addToTail);
Synth("reverb", [in: 20, out: 22, predelay: 0.048, combMinTime: 0.03, combDelayTime: 0.05, combDecay: 15, numAllpass: 6, allpassMinTime: 0.003, allpassDelayTime: 0.05, allpassDecaytime: 1, mix: 0.31], addAction:\addToTail);
Synth("volumeLine", [dur: 10, in: 20, startAmp: 1, endAmp: 1],addAction:\addToTail);
Synth("volumeLine", [dur: 10, in: 22, startAmp: 0.03, endAmp: 0.03],addAction:\addToTail);
)

/* Second variant
a.set(\predelay, 0.048)
a.set(\combdecay, 2.048)
a.set(\allpassdecay, 1.048)
a.set(\revVol, 0.048)
*/
(
Synth("pulse", [dur: 10, amp: 0.01, startFreq: 0.8, endFreq: 0.4, startWidth: 0.2, endWidth: 0.22, out: 16], addAction:\addToTail);
Synth("filt", [dur: 10, amp: 0.3, startFreq: 2000, endFreq: 3000, startBw: 0.000001, endBw: 0.000001, in: 16, out: 17], addAction:\addToTail);
Synth("limitReplace", [level: 0.3, in: 17], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: -1, endPan: 1, in: 17, out: 18], addAction:\addToTail);
Synth("reverb", [in: 18, out: 20, predelay: 0.048, combMinTime: 0.03, combDelayTime: 0.05, combDecay: 2.048, numAllpass: 6, allpassMinTime: 0.003, allpassDelayTime: 0.05, allpassDecaytime: 1.048, mix: 0.048], addAction:\addToTail);
Synth("volumeLine", [dur: 10, in: 18, startAmp: 1, endAmp: 1],addAction:\addToTail);
Synth("volumeLine", [dur: 10, in: 20, startAmp: 0.03, endAmp: 0.03],addAction:\addToTail);
)


/* Mix */
(
Synth("pulse", [dur: 12, startFreq: 3, endFreq: 5, startWidth: 0.1, endWidth: 0.9, out: 16], addAction:\addToTail);
Synth("filt", [dur: 12, startFreq: 7000, endFreq: 8500, startBw: 0.000001, endBw: 0.000001, in: 16, out: 17], addAction:\addToTail);
Synth("filt", [dur: 12, startFreq: 7500, endFreq: 7700, startBw: 0.000001, endBw: 0.000001, in: 16, out: 17], addAction:\addToTail);
Synth("limit", [level: 0.4, in: 17, out: 0], addAction:\addToTail);
)


/*Chain */
(
Synth("pulse", [dur: 12, startFreq: 3, endFreq: 5, startWidth: 0.1, endWidth: 0.9, out: 16], addAction:\addToTail);
Synth("filtReplace", [dur: 12, startFreq: 7000, endFreq: 8500, startBw: 0.000001, endBw: 0.000001, in: 16], addAction:\addToTail);
Synth("filtReplace", [dur: 12, startFreq: 7500, endFreq: 7700, startBw: 0.000001, endBw: 0.000001, in: 16], addAction:\addToTail);
Synth("limit", [level: 0.4, in: 18], addAction:\addToTail);
Synth("pan", [dur: 12, startPan: -1, endPan: 1, in: 16, out: 0], addAction:\addToTail);
)

/* Mix */
(
Synth("pinkNoise", [dur: 12, amp: 0.5, out: 16], addAction:\addToTail);
Synth("filt", [dur: 12, startFreq: 3000, endFreq: 4000, startBw: 0.0000001, endBw: 0.00001, in: 16, out: 0], addAction:\addToTail);
Synth("filt", [dur: 12, startFreq: 3030, endFreq: 3085, startBw: 0.0000001, endBw: 0.00001, in: 16, out: 0], addAction:\addToTail);
)

/* Chain */
(
Synth("pinkNoise", [dur: 12, amp: 0.5, out: 16], addAction:\addToTail);
Synth("filtReplace", [dur: 12, startFreq: 3000, endFreq: 4000, startBw: 0.0000001, endBw: 0.00001, in: 16], addAction:\addToTail);
Synth("filtReplace", [dur: 12, startFreq: 3030, endFreq: 3085, startBw: 0.0000001, endBw: 0.00001, in: 16], addAction:\addToTail);
Synth("pan", [dur: 12, startPan: -1, endPan: 1, in: 16, out: 0], addAction:\addToTail);
)

(
Synth("pulse", [dur: 12, startFreq: 3, endFreq: 5, startWidth: 0.1, endWidth: 0.9, out: 16], addAction:\addToTail);
Synth("pan", [dur: 12, startPan: -1, endPan: 1, in: 16, out: 17], addAction:\addToTail);
)

(
Synth("pulseASR", [dur: 10, amp: 1, out: 0,
	freqAttack: 20, freqSustain: 5, freqDecayStart: 3, freqDecayEnd: 10,
	freqAttackTime: 1, freqSustainTime: 8, freqDecayTime: 2,
	widthAttack: 0.1, widthSustain: 0.5, widthDecayStart: 0.4, widthDecayEnd: 0.9,
	widthAttackTime: 1, widthSustainTime: 5, widthDecayTime: 2], addAction: \addToTail);
)

(
Synth("pulse", [dur: 10, amp: 0.01, startFreq: 3, endFreq: 10, startWidth: 0.4, endWidth: 0.60, out: 16], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: -1, endPan: 1, in: 16, out: 18], addAction:\addToTail);
Synth("volumeAR", [dur: 10, in: 18],addAction:\addToTail);
)

(
Synth("pulse", [dur: 10, amp: 0.01, startFreq: 3, endFreq: 10, startWidth: 0.4, endWidth: 0.60, out: 16], addAction:\addToTail);
Synth("monoVolumeAR", [dur: 10, in: 16, out: 0],addAction:\addToTail);
)

(
Synth("pulse", [dur: 10, amp: 0.01, startFreq: 3, endFreq: 10, startWidth: 0.4, endWidth: 0.60, out: 0], addAction:\addToTail);
Synth("monoVolumeARReplace", [dur: 10, in: 0],addAction:\addToTail);
)

(
Synth("pinkNoise", [dur: 10, amp: 0.1, out: 16], addAction:\addToTail);
Synth("\monoVolumeLine", [dur: 10, in: 16, out: 0, startAmp: 0, endAmp: 0.1],addAction:\addToTail);
)

(
Synth("whiteNoise", [dur: 60, amp: 0.5, out: 0], addAction:\addToTail);
Synth("filtRejectReplace", [dur: 60, startFreq: 40, endFreq: 25, startBw: 1.8, endBw: 0.9, in: 0], addAction:\addToTail);
Synth("filtRejectReplace", [dur: 60, startFreq: 60, endFreq: 65, startBw: 0.8, endBw: 0.5, in: 0], addAction:\addToTail);
Synth("filtRejectReplace", [dur: 60, startFreq: 100, endFreq: 85, startBw: 0.3, endBw: 0.2, in: 0], addAction:\addToTail);
Synth("filtRejectReplace", [dur: 60, startFreq: 100, endFreq: 200, startBw: 0.8, endBw: 0.9, in: 0], addAction:\addToTail);
//Synth("pinkNoise", [dur: 60, amp: 0.2, out: 0], addAction:\addToTail);
)

(
Synth("pinkNoise", [dur: 60, amp: 0.2, out: 0], addAction:\addToTail);
)




(
Synth("pulse", [dur: 10, amp: 0.1, startFreq: 0.5, endFreq: 0.6, startWidth: 0.1, endWidth: 0.9, out: 16], addAction:\addToTail);
Synth("filt", [dur: 10, amp: 0.005, startFreq: 343, endFreq: 375, startBw: 0.0000001, endBw: 0.00000001, in: 16, out: 17], addAction:\addToTail);
Synth("monoDelayReplace", [dur: 10, in: 17, startDelay: 0.03, endDelay: 0.05], addAction:\addToTail);
Synth("monoCombReplace", [dur: 10, in: 17, startDelay: 0.07, endDelay: 0.09, startDecayTime: 0.6, endDecayTime: 0.9], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: 0.6, endPan: 0.3, in: 16, out: 0], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: 0.8, endPan: 1.0, in: 17, out: 0], addAction:\addToTail);
)
