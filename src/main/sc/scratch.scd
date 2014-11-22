
//Snävare bandvåg = mer ton (glas/metall)
(
Synth("pulseInstrument", [pulseFreq: 0.34, filtFreq: 70, filtBw: 0.00001, amp: 1, dur: 6 ]);
Synth("pulseInstrument", [pulseFreq: 0.13, filtFreq: 99, filtBw: 0.00001, amp: 1, dur: 6 ]);
)

(
Synth("pulseInstrument", [startPulseFreq: 5, endPulseFreq: 23, startFiltFreq: 7000, endFiltFreq: 9000, startFiltBw: 0.0000001, endFiltBw: 0.00001, amp: 1, dur: 13, startPan: -1, endPan: 1 ]);
Synth("pulseInstrument", [startPulseFreq: 7, endPulseFreq: 25, startFiltFreq: 6000, endFiltFreq: 8500, startFiltBw: 0.00001, endFiltBw: 0.0000001, amp: 1, dur: 13 ]);
Synth("noiseInstrument", [startFiltFreq: 200, endFiltFreq: 1000, startFiltBw: 0.001, endFiltBw: 0.0001, amp: 0.9, dur: 13, startPan: -1, endPan: 1 ]);
)

(
Synth("noiseInstrument", [startFiltFreq: 200, endFiltFreq: 1000, startFiltBw: 0.001, endFiltBw: 0.0001, amp: 0.9, dur: 13, startPan: -1, endPan: 1 ]);
)


/*
Impulse
{ Impulse.ar(13, 0.0, 0.5, 0) }.play
Pulse
{ Pulse.ar(13, 0.5, 0.2) }.play;
Try to combine with bandpass filter

See page 72
Pulse train followed by narrow bandpass
*/
