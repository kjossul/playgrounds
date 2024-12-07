clear all;
close all;
clc;

A = [12 5 -8 -5; -4 -4 8 -6; 4 2 -3 0; 0 -1 2 -4];
b = [4 -6 3 -3]';

[L, U] = lu(A);
y = L \ b;
x = U \ y;
assert(det(A) == det(L) * det(U));
