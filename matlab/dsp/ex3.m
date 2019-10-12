close all
clear vars
clc

A = [1.0, 0.75, 0.5, 0.25, 0.125];
F = [220, 440, 660, 880, 1100];
P = [0, 45, 90, 135, 180] .* pi ./ 180;  % convert to radians
F_s = 8000;
T_s = 1/F_s;
t = 0:T_s:1;


x6 = sum(A' .* cos(2 * pi * F' .* t + P'));
plot(x6);