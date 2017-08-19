// connect sine oscillator to D/A convertor (sound card)
SinOsc s => dac;

// machine
while( true ) {
    500::ms => now;
    Std.rand2f(30.0, 1000.0) => s.freq;
}