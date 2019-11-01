close all
clearvars
clc

B = [9, 7, -8, -3, 1, 1];
A = [2, -1, -2, 1];

[r, p, c_k] = residuez(B, A);

% h(n) using filter -> x(n) is the delta function

n_x = 0:100;
x = zeros(size(n_x));
x(1) = 1;
h1 = filter(B, A, x);

h2 = zeros(size(n_x));
h2(1:length(c_k)) = c_k;
for i=1:length(r)
    h2 = h2 + r(i) * p(i) .^ n_x;
end

figure(1);
stem(n_x, h1);
hold on;
stem(n_x, h2, '--');

% 11.b
clearvars

B = [-3, -2, 1, 6, 4, 1];
A = [-1, -1, 1, 1];

[r, p, c] = residuez(B, A);
n = 0:100;
delta = zeros(size(n));
delta(1) = 1;
h1 = filter(B, A, delta);

h2 = zeros(size(n));
h2(1:length(c)) = c;
for i=1:length(r)
    if i > 1 && (p(i) - p(i-1)) ^2 < 1e-6  % double multeplicity
        h2 = h2 + r(i) * (n+1).* p(i) .^n;
    else  % single multeplicity
        h2 = h2 + r(i) * p(i) .^ n;
    end
end

figure(2);
stem(n, h1);
hold on;
stem(n, h2, '--');
