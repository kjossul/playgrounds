close all
clearvars
clc

B = [1, -0.8];
A = 1;
N = 100;

[H_f, omega] = freqz(B, A, N, 'whole');

figure(1);
plot(omega - pi, fftshift(abs(H_f)));
title('Amplitude');

figure(2);
plot(omega - pi, fftshift(angle(H_f)));
title('Phase');

figure(3);
zplane(B, A);
title('Zeros and Poles');