close all
clearvars
clc

n_x = 0:4;
x = zeros(size(n_x));
x(3) = 1;
y = [5, 4, 3, 2, 1];
n_y = 0:length(y)-1;
N = length(y);

% linear convolution
n_z = n_x(1) + n_y(1) : n_x(end) + n_y(end);
z = conv(x, y);

% circular convolution

y_tilde = repmat(y, 1, 3);
n_y_tilde = -N:2*N-1;

z_tilde = conv(x, y_tilde);
n_z_tilde = n_x(1) + n_y_tilde(1) : n_x(end) + n_y_tilde(end);
z_c = z_tilde(n_z_tilde >= 0 & n_z_tilde < N);

figure(1);
stem(n_z, z);
title('Convolution');

figure(2);
stem(n_x, z_c);
hold on;
stem(n_x, cconv(x, y, N), '--');
title('Circular Convolution');