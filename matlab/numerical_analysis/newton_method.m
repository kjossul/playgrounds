function [x, x_iter] = newton_method(f, df, x0, tol, Nmax, p)
    for i=1:Nmax
        if i == 1
            x_iter(i) = x0;
        else
            x_iter(i) = x_iter(i-1) - p * f(x_iter(i-1)) / df(x_iter(i-1));
            if abs(x_iter(i) - x_iter(i-1)) < tol
                break
            end
        end
    end
    x = x_iter(i);
end

