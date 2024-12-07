function [H] = MyChol(A)
%MYCHOL Summary of this function goes here
%   Detailed explanation goes here
H = A * 0;
H(1,1) = sqrt(A(1,1));
for i=2:height(A)
    for j=1:i-1
        H(i,j) = (A(i,j) - H(i,1:j-1) * H(j,1:j-1)') / H(j,j);
    end
    H(i,i) = sqrt(A(i,i) - H(i, 1:i-1) * H(i,1:i-1)');
end
end

