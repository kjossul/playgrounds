close all
clearvars
clc

F_s = 8 * 1e3;
d = 1;
A = 1.5;
f = 1.1 * 1e3;
phi = 45 * pi / 180;
n = 0:1/F_s:d;
x = A * cos(2*pi*f*n + phi);

figure(1);
plot(n, x);
xlabel('t[s]');
title('Signal');

h = fir1(63, 0.8, 'low');
y = filter(h, 1, x);

N = 512;
w = hamming(N);
y_w = y(1:N) .* w';

Y_w = fft(y_w);
freq = F_s * 0:1/length(Y_w):1;
figure(2);
plot(freq(1:end-1) - F_s / 2, abs(fftshift(Y_w)));
