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

x1 = x(1:M:end);
cutoff = 1 / M;  % it's 1/2M but matlab requires to multiply by 2 
h = fir1(63, cutoff);
x2 = filter(h, 1, x);  % and then downsample
x2 = x2(1:M:end);

X = fft(x);
N = length(X);
X1 = fft(x1);
X2 = fft(x2);

freq = 0:F_s/N:F_s*(N-1)/N;  % remember that Fs is not included in the period

N1 = length(X1);
freq1 = 0:F_s/N1:F_s*(N1-1)/N1; 
figure(1);
plot(freq - F_s / 2, fftshift(abs(X)));
hold on;
plot(freq1 - F_s / 2, fftshift(abs(X1)), '--o');
hold on;
plot(freq1 - F_s / 2, fftshift(abs(X2)), '--x');
xlabel('f[Hz]');