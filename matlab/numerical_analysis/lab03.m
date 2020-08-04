%% Es 3.1
close all
clearvars
clc

fs = {@(x) 5 + x - x^2; @(x) 5 / x; @(x) 1 + x - (x^2)/5; @(x)(x + 5/x) / 2};
x0 = 2.2;
tol = 1e-6;
Nmax = 1e6;
for i=1:4
    z(i) = fixed_point(fs{i}, x0, tol, Nmax);
end

%% Es 3.2
close all
clearvars
clc

f = @(x) atan(7*(x - pi/2)) + sin((x - pi/2).^3);
df = @(x) 7/(49*(x-pi/2).^2 + 1) +3*(x-pi/2).^2 .* cos((x-pi/2).^3);
x = linspace(-1, 6, 1000);
figure(1);
plot(x, f(x));
grid on;
% [1, 2], z = pi/2, p=1
tol = 1e-10;
Nmax = 1e3;
p = 1;
[x1, x_iter1] = newton_method(f, df, 1.5, tol, Nmax, p);
[x2, x_iter2] = newton_method(f, df, 4, tol, Nmax, p);
hold on;
scatter(x1, 0);
% scatter(x2, 0);  % does not converge because 4 is way too far
a = -1;
b = 6;
Nbis = 5;
[x, x_iter] = bisection_newton(f, df, a, b, tol, Nbis, Nmax, p);
scatter(x, 0, 'x');

%% hw 3.1
close all
clearvars
clc

f1 = @(x) x - x.^3;
df1 = @(x) 1 - 3*x.^2;
f2 = @(x) x + x.^3;
df2 = @(x) 1 + 3*x.^2;
a = -1;
b = 1;
tol = 1e-9;
Nmax = 1e6;
[x1, x_iter1] = fixed_point(f1, 0.5, tol, Nmax);
[x2, x_iter2] = fixed_point(f2, -0.5, tol, Nmax);
% f2 diverges because it explodes

%% hw 3.2
close all
clearvars
clc

f = @(x) exp(x.^2).*log(x+1) - 1;
df = @(x) exp(x.^2) .* (2*x.*(x+1) .* log(x+1) + 1) ./ (x+1);
phi1 = @(x) sqrt(-log(log(x + 1)));
phi2 = @(x) x .* exp(x.^2) .* log(x+1);

tol = 1e-3;
Nmax = 1e6;
x = linspace(0.1, 1, 1e3);
figure(1);
plot(x, f(x));
grid on;
a = 0.7;
b = 0.8;
[x1, x_iter1] = fixed_point(phi1, a, tol, Nmax);
[x2, x_iter2] = fixed_point(phi2, a, tol, Nmax);  % diverges
[x3, x_iter3] = bisection_newton(f, df, a, b, tol, 5, Nmax, 1);