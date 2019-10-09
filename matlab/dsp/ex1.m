close all
clearvars
clc

T_s = 0.3;
t = 0:T_s:100;
omegas = [0.11, 0.09, 0.3];

y = sum(sin(omegas' * t / T_s));

figure(1);
plot(t, y);
grid;