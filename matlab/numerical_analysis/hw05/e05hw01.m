clc
clear all
close all
A = [1 2 1 1;
1 4 0 2;
2 10 4 0;
1 0 2 2];
b = [3 3 10 1]';

[L, U, P] = lu(A);
y = L \ (P * b);
x = U \ y;
assert(isequal(x, A \ b));
assert(det(A) == prod(diag(U)) / det(P));