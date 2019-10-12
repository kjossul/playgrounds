close all
clearvars
clc

n_x = -3:3;
x = [3, 11, 7, 0, -1, 4, 2];

% y(n) = x(n-5) => y(n) = x(n) * delta(n-5)
n_h = 0:10;
delta_5 = zeros(size(n_h));
delta_5(6) = 1;
y = conv(x, delta_5);
n_y = n_x(1) + n_h(1) : n_x(end) + n_h(end);

figure(1);
stem(n_y, y);