close all;
clear all;
clc;

A = [1 2 3 4; 0 1 2 3; 0 0 1 2; 0 0 0 1];
b = [1 1 1 1]';
x = backward_substitution(A,b);
assert(isequal(x, A \ b));
y = forward_substitution(A',b);
assert(isequal(y, A' \ b));
x1 = backward_substitution(A, y);
assert(isequal(x1, A' * A \ b));