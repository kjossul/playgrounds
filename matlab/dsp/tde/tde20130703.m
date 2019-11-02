close all
clearvars
clc

Fs = 512;
t = 0:1/Fs:1-1/Fs;

w = pi ./ [8, 10, 3];
x = sum(sin(2 * pi * w' .* t));

% upsample
L = 4;
x_up = zeros(1, L * length(x));
x_up(1:L:end) = x;

% interpolate
M = 4;
cutoff = min([1/L, 1/M]);
h = L * fir1(64, cutoff);
x_f = filter(h, 1, x_up);
y = x_f(1:M:end);

figure(1);
plot(t, x);
hold on;
plot(t, y, '--');