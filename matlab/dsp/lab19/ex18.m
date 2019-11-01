close all
clearvars
clc

f1 = 2;
f2 = 2.2;
N = 50;
F_s = 50;
n = 0:1/F_s:F_s;
n = n(1:N);

y1 = cos(2*pi*f1*n);
y2 = cos(2*pi*f2*n);
y = y1 + y2;

figure(1);
plot(n, y);
title('Signal');

Y = fft(y);
freq = 0:1/N*F_s: F_s - 1/N*F_s;

figure(2);
stem(freq - F_s/2, fftshift(abs(Y)));
title('Spectrum');

% Padding with zeros does not increase resolution, in order to exactly see
% the two peaks we must have a number of samples multiples of the two
% frequencies.