close all
clearvars
clc

Fs = 1;
N = 512;
t = 0:1/Fs:N-1/Fs;

w = pi ./ [8, 10, 3];
x = sum(cos(w' .* t));

L = 11;
M = 12;
x_up = zeros(1, length(x) * L);
x_up(1:L:end) = x;
cutoff = min([1/L, 1/M]);
h = fir1(63, cutoff);
x_i = L * filter(h, 1, x_up);
y = x_i(1:M:end);

figure(1);
subplot(2, 1, 1);
plot(x);
subplot(2, 1, 2);
plot(y);