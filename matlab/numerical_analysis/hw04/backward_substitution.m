function [x] = backward_substitution(A,b)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
n = numel(b);
x = b * 0;
x(n) = b(n) / A(n, n);
for k = n-1:-1:1
    x(k) = (b(k) - A(k, k+1:n) * x(k+1:n)) / A(k, k);
end
end

