close all;
clearvars;
clc;

%% a
f = @(x) 1 / 2 - x.^2 / factorial(4) + x.^4 / factorial(6) - x.^6 / factorial(8);

k = [1:30]';
x = 2.^(-k);
[x f(x)]

%% b
g = @(x) x./(sqrt(x + 1) + sqrt(x));
[x g(x)]
