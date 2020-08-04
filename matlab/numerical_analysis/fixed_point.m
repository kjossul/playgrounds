function [x, x_iter] = fixed_point(f, x0, tol, Nmax)
%FIXED_POINT Summary of this function goes here
%   Detailed explanation goes here
    x_iter(1) = x0;
    x_iter(2) = f(x0);
    for i=2:Nmax
        x_iter(i) = f(x_iter(i-1));
        if abs(x_iter(i) - x_iter(i-1)) < tol
            break
        end
    end
    x = x_iter(end);
end

