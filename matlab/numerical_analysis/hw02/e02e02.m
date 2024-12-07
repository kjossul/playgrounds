close all;
clear all;
clc;

f = @(x) sin(x) .* (1 - x) .^ 2;
df = @(x) cos(x) .* (1 - x) .^ 2 - 2 * sin(x) * (1 - x);
a = -0.5;
b = 1.5;
x = linspace(a, b, 1000);
grid on;
x0 = 0.3;
[z1, z_iter] = newton(f, df, x0, 1.e-6, 1000);
hold on;
plot(z1, 0, 'O');
y = f(z_iter);
hold on;
plot(z_iter, y, 'x');