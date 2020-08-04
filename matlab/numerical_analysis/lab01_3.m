function [m] = lab01_3(k)
    n = 2*(k+1);
    pow2 = 2 .^ (1 ./ (1:n));
    m = diag(pow2);
    v = lab01_2(k);
    row = zeros(1, n);
    row(1:2:end) = v;
    row(2:2:end) = v;
    col = zeros(n, 1);
    col(2:2:end) = v;
    m(end,:) = row;
    m(:,end) = col;
 end