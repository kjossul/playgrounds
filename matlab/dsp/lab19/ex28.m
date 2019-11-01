close all
clearvars
clc

F_s = 500;
d = 3;
f1 = 50;
f2 = 100;
n = 0:1/F_s:d;
M = 4;

x = cos(2*pi*f1*n) + cos(2*pi*f2*n);

x_d = x(1:M:end);
x1 = upsample(x_d, M);

cutoff = 1 / M;  % it's 1/2M but matlab requires to multiply by 2 
h = fir1(63, cutoff);
x_d1 = filter(h, 1, x);  % and then downsample
x_d1 = x_d1(1:M:end);

x2 = upsample(x_d1, M);
x2 = M * filter(h, 1, x2);  % remember multiplication for gain

N = 120;
figure(1);
subplot(3, 1, 1);
stem(n(1:N), x(1:N));

t_down = n(1:N/M) * M ;
subplot(3, 1, 2);
stem(t_down, x_d(1:length(t_down)), '--o');
hold on;
stem(t_down, x_d1(1:length(t_down)), '--x');

subplot(3, 1, 3);
stem(n(1:N), x1(1:N), '.');
hold on;
stem(n(1:N), x2(1:N), '--.');

% the last graph has a delay because filter needs to be causal (only start
% on positive values). Total delay is 33 + 33, since we are filtering two
% times with a filter of order 63 (64 samples, centre in 32)