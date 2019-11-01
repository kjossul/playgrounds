close all
clearvars
clc

F_s = 8 * 1e3;
d = 1;
A = 1.5;
f = 1.1 * 1e3;
phi = 45;
n = 0:1/F_s:d;
x = A * cos(2*pi*f*n + phi);

figure(1);
plot(n, x);
xlabel('t[s]');
title('Signal');

