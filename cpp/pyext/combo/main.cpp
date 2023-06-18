#include "choc_Oscillators.h"
#include <stdio.h>

#define SAMPLE_RATE 44100

int main()
{
	choc::oscillator::Sine<float> sine;
	choc::oscillator::Saw<float> saw;
	choc::oscillator::Square<float> sqr;
	choc::oscillator::Triangle<float> tri;

	sine.setFrequency(220.f, SAMPLE_RATE);
	saw.setFrequency(220.f, SAMPLE_RATE);
	sqr.setFrequency(220.f, SAMPLE_RATE);
	tri.setFrequency(220.f, SAMPLE_RATE);

	for (int i = 0; i < 4410; ++i) {
		printf("%d: %f %f %f %f\n", i, 
			sine.getSample(), saw.getSample(), sqr.getSample(), tri.getSample());
	}


}