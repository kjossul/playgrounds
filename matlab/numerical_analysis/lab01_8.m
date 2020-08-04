function [v1, v2] = lab01_8(n)
    v1 = zeros(1, n);
    v2 = zeros(1, n);
    v1(1) = 1;
    v1(2) = 1/3;
    v2(1) = 1;
    for i=1:n-1
        v1(i+2) = 10 * v1(i+1) / 3 - v1(i);
        v2(i+1) = v2(i) * 1/3;
    end
    v1 = v1(1:end-1);
end