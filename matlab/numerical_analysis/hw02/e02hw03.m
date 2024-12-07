close all;
clear all;
clc;

f = @(x) tan(x) - 2 * x;
df = @(x) 1 / cos(x) .^ 2 - 2;
x0 = 1;
[z, z_iter] = newton(f, df, x0, 1.e-15, 1000);
x = linspace(0, pi/2 -0.2, 1000);
plot(x, f(x));
grid on;
hold on;
plot(z, 0, 'O');
z
g = @(x) sin(x);
dg = @(x) cos(x);
x = linspace(-pi/2, pi/2, 1000);
[z, z_iter] = newton(g, dg, z, 1.e-15, 25);
z_iter
figure;
plot(x, g(x));
grid on;
hold on;
plot(z, 0, 'O');