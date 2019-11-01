close all
clearvars
clc

n_x = -3:3;
x = [3, 11, 7, 0, -1, 4, 2];
n_h = -1:4;
h = [2, 3, 0, -5, 2, 1];

n = -7:7;
x1 = zeros(1, length(n));
x1(5:5+length(x)-1) = x;
h1 = zeros(1, length(n));
h1(7:7+length(h)-1) = h;

y = conv(x, h);
n_y = n_x(1) + n_h(1) : n_x(end) + n_h(end);

figure(1);
stem(n_y, y)