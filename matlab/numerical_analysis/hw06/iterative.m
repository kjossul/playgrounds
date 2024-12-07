function [x] = iterative(B, g, x0, tol, max_it)
%ITERATIVE Summary of this function goes here
%   Detailed explanation goes here
x = x0;
for i=1:max_it
    if norm(g - B * x, 2) <= tol
        break;
    end
    x = B * x + g;
end
end

