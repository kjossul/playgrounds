close all
clearvars
clc

% y(n) = x(n) - ax(n-1) + by(n-1)
a = 1.2;
b = 1.1;
n = 0:50;
A = [1, -a];
B = [1, -b];

delta = zeros(size(n));
delta(1) = 1;
h = filter(B, A, delta);

% zeros and poles
zs = roots(B);
ps = roots(A);

figure(1);
zplane(zs, ps);
grid;

figure(2);
stem(n, h);