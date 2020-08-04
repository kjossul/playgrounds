function [x, x_iter] = bisection_newton(f, df, a, b, tol, Nbis, Nnew, p)
    [x0, x_iter_bis] = bisection(f, a, b, tol, Nbis);
    [x, x_iter] = newton_method(f, df, x0, tol, Nnew, p);
end

