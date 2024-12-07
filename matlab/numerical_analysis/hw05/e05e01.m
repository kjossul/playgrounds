clc
clear all
close all
E = [4 1 1 1 5;
4 1 2 0 0;
1 0 15 5 1;
0 2 4 10 2;
3 1 2 4 20];
b = [12 19 22 18 30]';
% Since one of the minor determinants is equal to zero, the LU
% factorization without pivoting does not exist.

[L, U, P] = lu(E);
y = L \ (P * b);
x = U \ y;
assert(isequal(x, E \ b));