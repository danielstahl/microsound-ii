
(
Synth("lineControl", [dur: 1, startValue: 0.4, endValue: 0.1, out: 0], addAction:\addToTail);
Synth("pinkNoise2", [dur: 1, ampBus: 0, out: 0], addAction:\addToTail);
)

(
Synth("lineControl", [dur: 1, startValue: 0.4, endValue: 0.1, out: 0], addAction:\addToTail);
Synth("whiteNoise", [dur: 1, ampBus: 0, out: 0], addAction:\addToTail);
)

(
Synth("lineControl", [dur: 10, startValue: 0, endValue: 0.5, out: 0], addAction:\addToTail);
Synth("lineControl", [dur: 10, startValue: 40, endValue: 40, out: 1], addAction:\addToTail);
Synth("lineControl", [dur: 10, startValue: 0.1, endValue: 0.9, out: 2], addAction:\addToTail);
Synth("pulse", [dur: 10, amp: 1, ampBus: 0, freqBus: 1, widthBus: 2, out: 0], addAction:\addToTail);
)

/* Moduler instruments */
(

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

SynthDef(\sineControlReplace, {
	arg dur, in = 0, startFreq = 1, endFreq = 1, startAmp = 1, endAmp = 1;
	var freqEnv, input, ampEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	ampEnv = Line.kr(startAmp, endAmp, dur);
	input = In.kr(in);
	ReplaceOut.kr(in, input + SinOsc.kr(freqEnv, 0, ampEnv));
}).add;

SynthDef(\pinkNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	noise = PinkNoise.ar();
	amp = In.kr(ampBus);
	Out.ar(out, noise * amp);
}).add;

SynthDef(\whiteNoise, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	noise = WhiteNoise.ar();
	amp = In.kr(ampBus);
	Out.ar(out, noise * amp);
}).add;

SynthDef(\pulse, {
	arg dur = 1, ampBus = 0, freqBus = 1, widthBus = 2, out = 0;
	var pulse, amp, freq, width;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	freq = In.kr(freqBus);
	width = In.kr(widthBus);
	pulse = Pulse.ar(freq, width);
	Out.ar(out, pulse * amp);
}).add;


SynthDef(\filt, {
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

SynthDef(\filtReplace, {
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

SynthDef(\highPass, {
	arg dur = 1, freqBus = 1, in = 0, out = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = HPF.ar(input, freq);
	Out.ar(out, filt);
}).add;

SynthDef(\highPassReplace, {
	arg dur = 1, freqBus = 1, in = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = HPF.ar(input, freq);
	ReplaceOut.ar(in, filt);
}).add;

SynthDef(\lowPass, {
	arg dur = 1, freqBus = 1, in = 0, out = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = LPF.ar(input, freq);
	Out.ar(out, filt);
}).add;

SynthDef(\lowPassReplace, {
	arg dur = 1, freqBus = 1, in = 0;
	var filt, freq, input;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus);
	input = In.ar(in, 1);
	filt = LPF.ar(input, freq);
	ReplaceOut.ar(in, filt);
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
	arg dur = 1, panBus = 0, in = 0, out = 0;
	var pan, input;
	Line.kr(dur:dur, doneAction:2);
	pan = In.kr(panBus);
	input = In.ar(in, 1);
	Out.ar(out, LinPan2.ar(input, pan));
}).add;

SynthDef(\stereoVolume, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 2);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolume, {
	arg dur = 1, ampBus = 0, in = 0, out = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	Out.ar(out, input * amp);
}).add;

SynthDef(\monoVolumeReplace, {
	arg dur = 1, ampBus = 0, in = 0;
	var amp, input;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, input * amp);
}).add;

SynthDef(\monoDelay, {
	arg dur = 1, in = 0, out = 0, delayBus = 0, maxDelay = 0.1;
	var delay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	input = In.ar(in, 1);
	Out.ar(out, DelayN.ar(input, maxDelay, delay));
}).add;

SynthDef(\monoDelayReplace, {
	arg dur = 1, in = 0, delayBus = 0, maxDelay = 0.1;
	var delay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	input = In.ar(in, 1);
	ReplaceOut.ar(in, DelayN.ar(input, maxDelay, delay));
}).add;


SynthDef(\monoComb, {
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
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);

	ReplaceOut.ar(in, CombL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\monoAllpass, {
	arg dur = 1, in = 0, out = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);
	Out.ar(out, AllpassL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\monoAllpassReplace, {
	arg dur = 1, in = 0,
	delayBus = 0, decayTimeBus = 1, maxDelay = 0.1;
	var delay, decay, input;
	Line.kr(dur:dur, doneAction:2);
	delay = In.kr(delayBus);
	decay = In.kr(decayTimeBus);
	input = In.ar(in, 1);

	ReplaceOut.ar(in, AllpassL.ar(input, maxDelay, delay, decay));
}).add;

SynthDef(\gverb, {
	arg dur = 1, in = 0, out = 0,
	roomSize, revTime, damping, inputBw,
	spread = 15, earlyLevel, tailLevel;
	var input;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 2);
	Out.ar(out,
		GVerb.ar(Mix.new(input),
		roomSize,
        revTime,
        damping,
        inputBw,
        spread,
        0,
        earlyLevel.dbamp,
        tailLevel.dbamp,
		roomSize, 0.3));
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
Synth("lineControl", [dur: 10, startValue: 0, endValue: 0.5, out: 0], addAction:\addToTail);
Synth("lineControl", [dur: 10, startValue: 40, endValue: 40, out: 1], addAction:\addToTail);
Synth("lineControl", [dur: 10, startValue: 0.1, endValue: 0.9, out: 2], addAction:\addToTail);
Synth("pulse", [dur: 10, amp: 1, ampBus: 0, freqBus: 1, widthBus: 2, out: 0], addAction:\addToTail);
)

(
Synth("lineControl", [dur: 30, startValue: 0, endValue: 0.5, out: 0], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 0.1, endValue: 0.25, out: 1], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 0.3, endValue: 0.7, out: 2], addAction:\addToTail);
Synth("pulse", [dur: 30, ampBus: 0, freqBus: 1, widthBus: 2, out: 16], addAction:\addToTail);

Synth("lineControl", [dur: 30, startValue: 0.005, endValue: 0.005, out: 3], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 343, endValue: 375, out: 4], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 0.0000001, endValue: 0.00000001, out: 5], addAction:\addToTail);
Synth("filt", [dur: 30, ampBus: 3, freqBus: 4, bwBus: 5, in: 16, out: 17], addAction:\addToTail);

Synth("arControl", [dur: 30, attackTime: 0.3, attackStart: 0.01, releaseStart: 0.1, releaseEnd: 0.02, out: 6], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 6, startFreq: 0.5, endFreq: 0.5, startAmp: 0.001, endAmp: 0.001], addAction:\addToTail);
Synth("monoDelayReplace", [dur: 30, in: 17, delayBus: 6, maxDelay: 0.05], addAction:\addToTail);

Synth("lineControl", [dur: 30, startValue: 0.07, endValue: 0.09, out: 7], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 7, startFreq: 0.7, endFreq: 0.3, startAmp: 0.01, endAmp: 0.008], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 0.6, endValue: 0.9, out: 8], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 8, startFreq: 0.6, endFreq: 0.5, startAmp: 0.05, endAmp: 0.06], addAction:\addToTail);
Synth("monoCombReplace", [dur: 30, in: 17, delayBus: 7, decayTimeBus: 8, maxDelay: 0.09], addAction:\addToTail);

Synth("lineControl", [dur: 30, startValue: 0.01, endValue: 0.009, out: 9], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 1.3, endValue: 1.4, out: 10], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 10, startFreq: 0.09, endFreq: 0.1, startAmp: 0.05, endAmp: 0.09], addAction:\addToTail);
Synth("monoAllpassReplace", [dur: 30, in: 17, delayBus: 9, decayTimeBus: 10, maxDelay: 0.09], addAction:\addToTail);

// 1443.8699 -> 667.2136
Synth("lineControl", [dur: 30, startValue: 0.005, endValue: 0.005, out: 13], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 1443, endValue: 667, out: 14], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 0.0000001, endValue: 0.00000001, out: 15], addAction:\addToTail);
Synth("filt", [dur: 30, ampBus: 13, freqBus: 14, bwBus: 15, in: 16, out: 18], addAction:\addToTail);

Synth("arControl", [dur: 30, attackTime: 0.3, attackStart: 0.01, releaseStart: 0.1, releaseEnd: 0.02, out: 16], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 16, startFreq: 0.5, endFreq: 0.5, startAmp: 0.001, endAmp: 0.001], addAction:\addToTail);
Synth("monoDelayReplace", [dur: 30, in: 18, delayBus: 16, maxDelay: 0.05], addAction:\addToTail);


Synth("lineControl", [dur: 30, startValue: 0.1, endValue: 0.2, out: 17], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 17, startFreq: 0.7, endFreq: 0.3, startAmp: 0.01, endAmp: 0.008], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 1.2, endValue: 1.3, out: 18], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 8, startFreq: 0.6, endFreq: 0.5, startAmp: 0.05, endAmp: 0.06], addAction:\addToTail);
Synth("monoCombReplace", [dur: 30, in: 18, delayBus: 17, decayTimeBus: 18, maxDelay: 0.09], addAction:\addToTail);

Synth("lineControl", [dur: 30, startValue: 0.02, endValue: 0.012, out: 19], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 1.6, endValue: 1.9, out: 20], addAction:\addToTail);
Synth("sineControlReplace", [dur: 30, in: 20, startFreq: 0.09, endFreq: 0.1, startAmp: 0.05, endAmp: 0.09], addAction:\addToTail);
Synth("monoAllpassReplace", [dur: 30, in: 18, delayBus: 19, decayTimeBus: 20, maxDelay: 0.09], addAction:\addToTail);

Synth("lineControl", [dur: 30, startValue: 0.6, endValue: 0.3, out: 11], addAction:\addToTail);
Synth("pan", [dur: 30, panBus: 11, in: 16, out: 0], addAction:\addToTail);
Synth("lineControl", [dur: 30, startValue: 0.8, endValue: 1.0, out: 12], addAction:\addToTail);
Synth("pan", [dur: 30, panBus: 12, in: 17, out: 0], addAction:\addToTail);

Synth("lineControl", [dur: 30, startValue: 0.7, endValue: 0.8, out: 17], addAction:\addToTail);
Synth("pan", [dur: 30, panBus: 17, in: 18, out: 0], addAction:\addToTail);

)
