clc
clear all
close all
% Matrix definition. Uncomment the one you want to analyze
% A = [1 2 2; 1 1 1; 2 2 1]; % None of the methods converge
A = [2 1 -2; 1 2 1; 2 1 2]; % Both methods converge
% A = [1 2 −2; 1 1 1; 2 2 1]; % Just Jacobi method converges
% A = [1 1 −2; 1 2 1; 2 1 2]; % Just Gauss−Seidel method converges
x = [1 2 3]'; % Exact solution
b = A*x; % Right hand side

% Iteration matrices.
D = diag( diag(A) );
L = tril(A, -1);
U = triu(A, 1);
Bj = -D \ (L+U); % Jacobi method
Bgs = -(D+L) \ U; % Gauss−Seidel method

gj = D \ b;
ggs = (D+L) \ b;
x0 = [0 0 0]';
tol = 1e-6;
maxit = 100;
% Jacobi method
[xj] = iterative(Bj, gj, x0, tol, maxit)
% Gauss−Seidel method
[xgs] = iterative(Bgs, ggs, x0, tol, maxit)