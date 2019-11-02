close all
clearvars
clc

Fs = 80e3;
f1 = 10e3;
f2 = 40e3;
N = 1024;
t = linspace(0, (N-1) / Fs, N);

x = 5 + 3 * cos(2*pi*f1*t) + cos(2*pi*f2*t);

Ft = 120e3;
[L, M] = rat(Ft / Fs);

% upsample
x_up = zeros(1, length(x) * L);
x_up(1:L:end) = x;

% LP filter
cutoff = min(1/L, 1/M);  % it should be 1/2L but matlab needs 2x
h = L * fir1(63, cutoff);
x_f = filter(h, 1, x_up);

% downsample
y = x_f(1:M:end);

t_y = t * L / M;
NY = 512;

figure(1);
subplot(2, 1, 1);
plot(t, x);
xlabel('t[s]');
ylabel('x');
subplot(2, 1, 2);
plot(t_y(1:NY), y(1:NY));
xlabel('t[s]');
ylabel('y');