close all
clearvars
clc

%% 2.1

f = @(x) x.^3 - (2+exp(1))*x.^2 + (2*exp(1)+1)*x + (1-exp(1)) - cosh(x-1);
a = 0.5;
b = 5.5;
x = linspace(a, b, 1000);
figure(1);
plot(x,f(x));
grid on;
% needed iterations:
tol = 10^-3;
it = log((b-a)/tol) / log(2);
[x, x_iter] = lab02_1(f, 3, 5, tol);

%% 2.2
close all
clearvars
clc
f = @(x) sin(x) .* (1-x).^2;
a = -0.5;
b = 1.5;
x = linspace(a, b, 1000);
figure(2);
plot(x, f(x));
grid;
df = @(x) cos(x) .* (1-x).^2 - sin(x) .* 2 * x;
tol = 1e-6;
[x1, x_iter1] = lab02_2(f, df, 0.3, tol, 1e6);
[x2, x_iter2] = lab02_2(f, df, 0.5, tol, 1e6);
%% ex 2.1
close all
clearvars
clc

f = @(x) (2 * x .* cot(x) - x.^2 + 1) ./ (2*x);
a = 6.5;
b = 7;
x = linspace(a, b, 1000);
figure(1);
plot(x, f(x))
grid on;
% [1, 1.5], [3.6, 3.7], [6.5, 6.6]
tol = 1e-6;
i = 1;
while b-a > tol
    z = (a+b) / 2;
    if f(z)*f(a) < 0
        b = z;
    else
        a = z;
    end
    z_iter(i) = z;
    i = i+1;
end
hold on;
scatter(z, 0);
figure(2);
plot(z_iter - z);
hold on;

%% hw 2.2
close all
clearvars
clc

f = @(x) exp(-(x-2).^2) - 1 + exp(x-4);
x = linspace(-1, 6, 1e6);
plot(x, f(x));
grid;
a1 = 0; b1 = 5;
a2 = 1; b2 = 6;
a3 = 1; b3 = 5;
a4 = -1; b4 = 5;
tol = 1e-3;
[z1, z_iter1] = lab02_1(f, a1, b1, tol);
[z2, z_iter2] = lab02_1(f, a2, b2, tol);
[z3, z_iter3] = lab02_1(f, a3, b3, tol);
[z4, z_iter4] = lab02_1(f, a4, b4, tol);
hold on;
scatter([z1, z2, z3, z4], zeros(1, 4));

%% hw 2.3
close all
clearvars
clc

