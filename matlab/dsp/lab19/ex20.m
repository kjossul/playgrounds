close all
clearvars
clc

n = 0:5;
N = length(n);
x = ones(size(n));
y = x;

z1 = conv(x, y);

N_pad = N + N - 1;
x_pad = padarray(x, [0, N_pad-N], 'post');
y_pad = padarray(y, [0, N_pad-N], 'post');
z2 = cconv(x_pad, y_pad);
n_z = 0 : n(end) * 2;
z2 = z2(n_z >= 0 & n_z < N_pad);

X = fft(x_pad);
Y = fft(y_pad);
Z = X .* Y;
z = ifft(Z);

figure(1);
stem(n_z, z2);
hold on;
stem(n_z, z, '--');