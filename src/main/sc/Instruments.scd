
/* Moduler instruments */
(
SynthDef(\noise, {
	arg dur = 1, amp = 0.5, out = 0;
	var noise;
	noise = PinkNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\pulse, {
	arg dur = 1, amp = 1, startFreq, endFreq, startWidth, endWidth, out = 0;
	var pulse, freqEnv, widthEnv;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	widthEnv = Line.kr(startWidth, endWidth, dur);
	pulse = Pulse.ar(freqEnv, widthEnv);
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

SynthDef(\filtRA, {
	arg dur = 1, amp = 1, startFreq, endFreq, startBw, endBw, in = 0;
	var filt, freqEnv, bwEnv, input;
	freqEnv = Line.kr(startFreq, endFreq, dur, doneAction:2);
	bwEnv = Line.kr(startBw * startFreq, endBw * endFreq, dur);
	input = In.ar(in, 1);
	filt = BPF.ar(input, freqEnv, bwEnv);
	filt = Balance.ar(filt, input);
	ReplaceOut.ar(in, filt * amp);
}).add;

SynthDef(\limit, {
	arg level = 1, dur = 0.01, in = 0, out = 0;
	var input;
	input = In.ar(in, 1);
	Out.ar(out, Limiter.ar(input, level, dur));
}).add;

SynthDef(\limitRA, {
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

SynthDef(\arVolume, {
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

SynthDef(\lineVolume, {
	arg dur = 1, startAmp = 1, endAmp = 1, in = 0, out = 0;
	var ampEnv, input;
	ampEnv = Line.kr(startAmp, endAmp, dur, doneAction:2);

	input = In.ar(in, 2);
	Out.ar(out, input * ampEnv);
}).add;

SynthDef(\reverb, {
	arg in = 0, out = 0, predelay = 0.048, combMinTime = 0.01, combDelayTime = 0.1, combDecay = 5,
	numAllpass = 6, allpassMinTime = 0.001 ,allpassDelayTime = 0.05, allpassDecaytime = 1;
	var input, numc, temp, numComb;

	input=In.ar(in, 2); //get two channels of input

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
Synth("lineVolume", [dur: 10, in: 20, startAmp: 1, endAmp: 1],addAction:\addToTail);
Synth("lineVolume", [dur: 10, in: 22, startAmp: 0.03, endAmp: 0.03],addAction:\addToTail);
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
Synth("lineVolume", [dur: 10, in: 20, startAmp: 1, endAmp: 1],addAction:\addToTail);
Synth("lineVolume", [dur: 10, in: 22, startAmp: 0.03, endAmp: 0.03],addAction:\addToTail);
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
Synth("limitRA", [level: 0.3, in: 17], addAction:\addToTail);
Synth("pan", [dur: 10, startPan: -1, endPan: 1, in: 17, out: 18], addAction:\addToTail);
Synth("reverb", [in: 18, out: 20, predelay: 0.048, combMinTime: 0.03, combDelayTime: 0.05, combDecay: 2.048, numAllpass: 6, allpassMinTime: 0.003, allpassDelayTime: 0.05, allpassDecaytime: 1.048, mix: 0.048], addAction:\addToTail);
Synth("lineVolume", [dur: 10, in: 18, startAmp: 1, endAmp: 1],addAction:\addToTail);
Synth("lineVolume", [dur: 10, in: 20, startAmp: 0.03, endAmp: 0.03],addAction:\addToTail);
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
Synth("filtRA", [dur: 12, startFreq: 7000, endFreq: 8500, startBw: 0.000001, endBw: 0.000001, in: 16], addAction:\addToTail);
Synth("filtRA", [dur: 12, startFreq: 7500, endFreq: 7700, startBw: 0.000001, endBw: 0.000001, in: 16], addAction:\addToTail);
Synth("limit", [level: 0.4, in: 18], addAction:\addToTail);
Synth("pan", [dur: 12, startPan: -1, endPan: 1, in: 16, out: 0], addAction:\addToTail);
)

/* Mix */
(
Synth("noise", [dur: 12, amp: 0.5, out: 16], addAction:\addToTail);
Synth("filt", [dur: 12, startFreq: 3000, endFreq: 4000, startBw: 0.0000001, endBw: 0.00001, in: 16, out: 0], addAction:\addToTail);
Synth("filt", [dur: 12, startFreq: 3030, endFreq: 3085, startBw: 0.0000001, endBw: 0.00001, in: 16, out: 0], addAction:\addToTail);
)

/* Chain */
(
Synth("noise", [dur: 12, amp: 0.5, out: 16], addAction:\addToTail);
Synth("filtRA", [dur: 12, startFreq: 3000, endFreq: 4000, startBw: 0.0000001, endBw: 0.00001, in: 16], addAction:\addToTail);
Synth("filtRA", [dur: 12, startFreq: 3030, endFreq: 3085, startBw: 0.0000001, endBw: 0.00001, in: 16], addAction:\addToTail);
Synth("pan", [dur: 12, startPan: -1, endPan: 1, in: 16, out: 0], addAction:\addToTail);
)

(
Synth("pulse", [dur: 12, startFreq: 3, endFreq: 5, startWidth: 0.1, endWidth: 0.9, out: 16], addAction:\addToTail);
Synth("pan", [dur: 12, startPan: -1, endPan: 1, in: 16, out: 17], addAction:\addToTail);
)
