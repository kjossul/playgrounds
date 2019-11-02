close all
clearvars
clc

x = [4, 3, 2, 6, 4, 3];
h = [0, 2, 1];

n_x = 0:length(x) - 1;
n_h = 0:length(h) - 1;

y = conv(x, h);
n_y = 0:n_x(end) + n_h(end);

N = length(x) + length(h) - 1;
X = fft(x, N);
H = fft(h, N);
Y2 = X .* H;
y2 = ifft(Y2);

mse = (y - y2) .^ 2;

figure(1);
plot(n_y, mse);
title('MSE');

figure(2);
plot(n_y, y);
hold on;
plot(n_y, y2, '--');
title('y and y2');