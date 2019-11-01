close all
clearvars
clc

n = 0:19;
N = length(n);
h_n = ones(size(n));
H_k = fft(h_n);

zeroz = 100 - N;
h_padded = padarray(h_n, [0, zeroz], 'post');
n2 = 0:length(h_padded)-1;
H_padded = fft(h_padded);

figure(1);
stem(n ./ N - 0.5, fftshift(abs(H_k)));
hold on;
stem(n2 ./ length(h_padded) - 0.5, fftshift(abs(H_padded)));
