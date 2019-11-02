close all
clearvars
clc

A = [1.0, 0.75, 0.5, 0.25, 0.125];
F = 100 * [2, 4, 6, 8, 10];
P = pi / 180 * [0, 0, 90, 90, -90];

Fs = 44.1e3;
d = 1;
t = 0:1/Fs:d;

xs = A' .* cos(2 * pi * F' .* t + P');
x6 = sum(xs);

N = 512;
h = hamming(N);
y6 = x6(1:N) .* h';

figure(1);
plot(t(1:N), y6);
xlabel('t[s]');

% W matrix multiplication

w = exp(-1i * 2 * pi / N);
n = 0:N-1;
W = w .^ (n .* n');
Y6_a = (W * y6')';

Y6_b = fft(y6, N);

errors = abs(Y6_a - Y6_b);

figure(2);
plot(n, errors);


