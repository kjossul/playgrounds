function [x, x_iter] = bisection(f, a, b, tol, Nbis)
    for i=1:Nbis
        x = (a+b) / 2;
        x_iter(i) = x;
        y = f(x);
        if y * f(a) < 0
            b = x;
        else
            a = x;
        end
        if b-a < tol
           break
        end
    end
end

