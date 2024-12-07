function [x] = bisection(f, a, b, tol)
%BISECTION Summary of this function goes here
%   Detailed explanation goes here
x = (b + a) / 2;
while abs(f(x)) > tol
    if f(x) * f(a) < 0
        b = x;
    else
        a = x;
    end
    x = (b + a) / 2;
end
