close all
clearvars
clc

rho = 0.9;
theta = pi / 8;

A = [1, 2, 1];
B = [1, 2*rho*cos(theta), rho^2];
N = 1e4;
n = 0:N-1;

delta = zeros(size(n));
delta(1) = 1;
h_n = filter(B, A, delta);
[H_f, omega] = freqz(B, A, N, 'whole');

W = exp(-1i * 2 * pi / N * n' .* n);
H_k = W * h_n';
figure(1);
semilogy(omega, abs(H_f));
title('Amplitude');
hold on;
semilogy(omega, abs(H_k));
hold on;
semilogy(omega, abs(fft(h_n)), '--');