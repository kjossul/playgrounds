close all;
clc;
clear all;

f= @(x) x.^3-(2 + exp(1))*x.^2 + (2*exp(1) + 1)*x + (1 - exp(1)) - cosh(x - 1);
a = 3;
b = 5;
tol = 1.e-3;
zero = bisection(f, a, b, tol);
x = linspace(a, b, 1000);
plot(x, f(x));
grid on;
hold on;
plot(zero, 0, 'O');