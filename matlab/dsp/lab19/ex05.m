close all
clearvars
clc

n = 1:15;
x1 = zeros(15,1);
x1(5:end) = 1;  % u(n-5)
x2 = zeros(15,1);
x2(10:end) = 1;  % u(n-10)
x = x1 - x2;
% also x1(n>=5 & n<10) = 1;

T = 15;
maxp = floor(200/15);
xp = repmat(x, 1, maxp+1);
xp = xp(1:200);

stem(xp);
grid;