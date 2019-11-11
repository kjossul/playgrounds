close all
clearvars
clc

z = [-1, -1i, +1i];
b = poly(z);
delta = zeros(size(b));
delta(1) = 1;
h = filter(b, 1, delta);

x = [1, -2, -2, 1];
Nfft = 4;
X = fft(x, Nfft);
H = fft(h, Nfft);
Y = X .* H;
y = ifft(Y);

figure(1);
freq = (0:Nfft-1) * 2 * pi / Nfft; 
subplot(2, 1, 1);
stem(freq, abs(X));
hold on;
stem(freq, abs(H), '--x');
hold on;
stem(freq, abs(Y), '--+');
hold on;
subplot(2, 1, 2);
stem(freq, angle(X));
hold on;
stem(freq, angle(H), '--X');
hold on;
stem(freq, angle(Y), '--+');
hold on;

figure(2);
plot(0:Nfft-1, y);