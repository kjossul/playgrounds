close all
clearvars
clc

n_x = 0:20;
x = n_x + ones(size(n_x));

n_h = 0:2;
h = [1, 0, -1];

c = conv(x, h);
n_c = 0:n_x(end) + n_h(end);

% overlap and add

L = 6;
M = length(n_h);
N = M + L - 1;

h_pad = padarray(h, [0, L-1], 'post');
H_pad = fft(h_pad);
oa = zeros(size(n_c));

for i=0:floor(length(n_x) / L)
    x_k = x(i*L +1: min((i+1)*L, n_x(end)+1));
    x_k = padarray(x_k, [0, (L-length(x_k)) + M-1], 'post');
    X_k = fft(x_k);
    Y_k = X_k .* H_pad;
    y_k = ifft(Y_k);
    slice = i*L+1 : min(i*L + (L+M-1), n_c(end)+1);
    oa(slice) = oa(slice) + y_k(1:length(slice));
end

% overlap and save

os = zeros(size(n_c));
x1 = padarray(x, [0, M-1], 'pre');
% add M-1 elements to each xk sector long L on the left (zeros for 1st block)
for i=0:floor(length(n_x) / L)
    x_k = x1(i*L+1:min((i+1)*L + (M-1), n_x(end)+1 + M-1));
    y_k = cconv(x_k, h_pad, M + L - 1);
    y_k = y_k((M-1) + 1: end);
    slice = i*L + 1: (i+1)*L;
    os(slice) = y_k;
end
os = os(1:length(oa));

figure(1);
stem(n_c, c);
hold on;
stem(n_c, oa, '--');
stem(n_c, os, ':');
title('Convolutions');