close all;
clear all;
clc;

n = 75;
B = -5*diag(ones(n, 1)) + diag(ones(n-1, 1), -1) + diag(ones(n-1, 1), 1);
A = B'*B;
xex = ones(n, 1);
b = A * xex;
tol = 1e-08;
nmax = 200;
x0 = zeros(n, 1);
% jacobi
D = diag(diag(A));
L = tril(A, -1);
U = triu(A, 1);
Bj = -D \ (L + U);
gj = D \ b;
rhobj = max ( abs(eig(Bj)));  % verified < 1
[xj, x_iterj] = stationary_method(Bj, gj, x0, tol, nmax);
% gauss siedel
PGS = D + L;
P = 0.5*(PGS + PGS');
T = inv(P) * A;
alphaopt = 2 / (min(eig(T)) + max(eig(T)));
