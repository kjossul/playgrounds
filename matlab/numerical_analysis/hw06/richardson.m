function [x] = richardson(A, P, b, alpha, x0, tol, max_it)
%RICHARDSON Summary of this function goes here
%   Detailed explanation goes here
x = x0;
for i=1:max_it
    r = b - A * x;
    if norm(r, 2) <= tol
        break;
    end
    x = x + alpha / P * r;
end
end

