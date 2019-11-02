close all
clearvars
clc

rho = rand(1);
theta = rand(1) * pi;
a = 2 * rho * cos(theta);
b = -rho^2;
c = - rand(1) ;

Bz = [1, c];
Az = [1, -a, -b];
z = roots(Bz);
p = roots(Az);
figure(1);
zplane(z, p);

[H, f] = freqz(Bz, Az);
figure(2);
plot(f, abs(H));

n = 0:100;
delta = zeros(size(n));
delta(1) = 1;
h = filter(Bz, Az, delta);
figure(3);
stem(n, h, '.-');

% 4
x = sin(0.1 * pi * n);
y = conv(x, h);
y1 = filter(Bz, Az, x);
figure(4);
plot(n, y(1:length(n)));
hold on;
plot(n, y1, '--');