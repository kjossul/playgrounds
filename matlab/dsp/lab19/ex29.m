close all
clearvars
clc

Fs = 8000;
A1 = 0.7;
A2 = 0.5;
f1 = 1800;
f2 = 3600;
d = 0.5;
t = 0:1/Fs:d;

x = A1*cos(2*pi*f1*t) + A2*cos(2*pi*f2*t);

Fs2 = 6000;

[L, M] = rat(Fs2/Fs);

x_u = interp(x, L);
y = x_u(1:M:end);