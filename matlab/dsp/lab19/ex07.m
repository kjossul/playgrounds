close all
clearvars
clc

n = 1e4;
g = randn(1, n);

m = mean(g);
sigma = var(g);

a = 0.1;
b = 4;

h = a * g + b;

m_h = mean(h);
sigma_h = var(h);