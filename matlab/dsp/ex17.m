close all
clearvars
clc

f = 2;
d = 1.3;
F_s = 50;
T_s = 1/F_s;
N = 50;
t = 0:T_s:d;
n = t(1:N);
y = sin(2*pi*f*t);
y_n = y(1:N);

figure(1);
plot(n, y_n);
title('signal');

Y_k = fft(y_n);
freq = 0:1/N*F_s: F_s - 1/N*F_s;

figure(2);
stem(freq - F_s / 2, fftshift(abs(Y_k)));
title('Sampled spectrum');

Y2_k = fft(y);
N2 = length(Y2_k);
freq2 = 0:1/N2*F_s:F_s - 1/N2*F_s;

figure(3);
stem(freq2 - F_s / 2, fftshift(abs(Y2_k)));
title('Whole Spectrum');

N_tot = 1e4;
n_pad = N_tot - N2;
y_pad = padarray(y, [0, n_pad], 'post');
Y_pad = fft(y_pad);

N_pad = length(Y_pad);
freq3 = 0: 1/N_pad*F_s : F_s - 1/N_pad * F_s;

figure(4);
stem(freq3 - F_s / 2, fftshift(abs(Y_pad)));
title('Padded Spectrum');


