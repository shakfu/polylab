gcc -std=c99 -o fft_c fft_c.c -lm
ldc fft_d.d
go build fft_go.go
strip fft_c fft_d fft_go
rm *.o

