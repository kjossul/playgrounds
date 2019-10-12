close all
clearvars
clc

A = 10;
F_s = 11025;
t = 0:1/F_s:2;

noise = 2 * A * rand(size(t)) - A;

f_x = 220;
x = cos(2*pi*220*t);

y = x + noise;
M = max(y);
m = min(y);

% normalization between [-1, 1] --> calculate the line passing for the two
% points (m, -1) and (M, 1) 
y = 2 / (M - m) * (y - m) - 1;  

plot(t(1:200), y(1:200));