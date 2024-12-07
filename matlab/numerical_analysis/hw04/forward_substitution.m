function [x] = forward_substitution(A, b)
%FORWARD_SUBSTITUTION Summary of this function goes here
%   Detailed explanation goes here
n = numel(b);
x = b * 0;
x(1) = b(1) / A(1,1);
for k=2:n
    x(k) = (b(k) - A(k, 1:k-1) * x(1:k-1)) / A(k, k);
end
end

