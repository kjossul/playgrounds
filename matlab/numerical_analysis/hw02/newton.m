function [x,x_iter] = newton(f, df, x0, tol, Nmax)
%NEWTON Summary of this function goes here
%   Detailed explanation goes here
x_iter(1) = x0;
for i=2:Nmax
    x_iter(i) = x_iter(i-1) - f(x_iter(i-1)) / df(x_iter(i-1));
    if abs(x_iter(i) - x_iter(i-1)) <= tol
        break;
    end
end
x = x_iter(end);
end

