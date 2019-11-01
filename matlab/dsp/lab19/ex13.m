close all
clearvars
clc

r = 0.9;
t = pi / 8;
A = [1, 2, 1];
B = [1, 2*r*cos(t), r^2];

zs = roots(B);
ps = roots(A);
n = 0:200;
delta = zeros(size(n));
delta(1) = 1;

h1 = filter(B, A, delta);
[r, p, c] = residuez(B, A);

h2 = zeros(size(n));
h2(1:length(c)) = c;

for i=1:length(r)
   if i > 1 && (p(i) - p(i-1)) < 1e-4
       h2 = h2 + r(i) * (n+1) .* p(i) .^ n;
   else
       h2 = h2 + r(i) * p(i) .^ n;
   end
end

figure(1);
zplane(zs, ps);
grid;

figure(2);
stem(n, h1);
hold on;
stem(n, h2, '--');