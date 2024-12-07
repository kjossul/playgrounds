close all;
clear all;
clc;

n = 25;
C = diag(5*ones(n, 1)) - diag(ones(n-1, 1), 1) - diag(ones(n-1, 1), -1);
A = tril(C);
b = ones(n, 1);

[x] = forward_substitution(A,b)