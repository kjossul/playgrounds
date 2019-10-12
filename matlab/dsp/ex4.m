close all
clearvars
clc

n = 1:20;
u = ones(1, 20);
x = 0.8 .^ n;

y1 = circshift(x, 5);
y1(1:5) = 0;
y2 = circshift(x, -5);
y2(end-5:end) = 0;

stem(n, x);
hold on;
stem(n, y1);
hold on;
stem(n, y2);