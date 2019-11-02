close all
clearvars
clc

p = [0.99, 0.9 * exp(1i * pi / 8), 0.9 * exp(-1i * pi / 8)];
z = [-1, exp(1i * pi / 16), exp(-1i * pi / 16)];

b = poly(z);
a = poly(p);

figure(1);
zplane(z', p');

[H, freq] = freqz(b, a, 'whole');
figure(2);
subplot(2, 1, 1);
plot(freq, abs(H));
subplot(2, 1, 2);
plot(freq, angle(H));

n = 0:10024;
delta = zeros(size(n));
delta(1) = 1;
h = filter(b, a, delta);

figure(3);
plot(n, h);