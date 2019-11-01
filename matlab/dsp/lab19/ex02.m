close all
clear vars
clc

F_s = 1000;
A = 0.8;
f = 50;
T_s = 1 / F_s;
t = 0:T_s:0.5;

x = A * cos(2 * pi * f * t);
x1 = sinusoid(t,A,f, pi / 2);

% plot(x) -> plot as a function of x
% plot(t, x) -> plot as a function of time samples
plot(t, x);
hold on;
plot(t, x1, '--');
grid;