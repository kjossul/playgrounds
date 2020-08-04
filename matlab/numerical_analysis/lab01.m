close all
clearvars
clc

%% 1.1
k = 8;
v1 = zeros(1, k+1);
for i=0:k
    v1(i+1) = (2*i+1)^2;
end
v2 = (2 * (0:k) + 1).^2;

%% 1.2
v3 = lab01_2(8);

%% 1.3
m = lab01_3(8);


%% 1.7
f = @(x)(exp(x) - 1) ./ x;
a1_7 = f(10.^-(1:20));
% for x = 10^-16 it becomes zero because we are below eps

f_taylor_5 = @(x) 1 + 1/2*x + 1/6*x.^2 + 1/24*x.^3 + 1/120*x.^4;

%% 1.8
[v1, v2] = lab01_8(35);
figure(1);
plot(v1);
hold on;
plot(v2, '--');