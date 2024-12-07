clc
clear all
close all
A = [10 0 3 0;
0 5 0 -2;
3 0 5 0;
0 -2 0 2];
assert(all(eig(A)>0));
% All the eigenvalues are positive => A is spd (the simmetry is trivial).
b = [2 2 2 2]';
H = chol(A)';
detA = (prod(diag(H)))^2;  % == det(A)