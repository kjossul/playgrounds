close all
clearvars
clc

rho = 1.2;
theta = 0.4 * pi;
z1 = rho * exp (1i * theta);
z2 = rho * exp (-1i * theta);
z = [z1, z2];
p = 1 ./ conj(z);
b = poly(z) ./ rho^2;
a = poly(p);

figure(1);
zplane(b, a);
[H_f, omega] = freqz(b, a);
figure(2);
subplot(2, 1, 1);
plot(omega, abs(H_f));
title('Amplitude');
subplot(2, 1, 2);
plot(omega, angle(H_f));
title('Phase');

N = 512;
delta = zeros(1, N);
delta(1) = 1;
h = filter(b, a, delta);
figure(3);
stem(0:N-1, h, '.-');
