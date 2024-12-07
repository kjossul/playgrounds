close all;
clear all;
clc;

f = @(x) (x.^2 - 1) - 2 * x .* cot(x);
x = linspace(0, 10, 1000);
plot(x, cot(x));
grid on;

a = 6.5;
b = 9;
z = bisection(f, a, b, 1.e-6);
hold on;
plot(z, 0, 'O');