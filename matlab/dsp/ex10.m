close all
clearvars
clc

n_x = -2:2;
x = [3, 2, 1, 0, 1];
n_h = 0:4;
h = [1, 3, 2.5, 4, 2];

n_y = n_x(1) + n_h(1) : n_x(end) + n_h(end);
y = conv(x, h);

% z transform : sum x(n) * r * e^(-jwn)
% same as fourier transform if r = 1 (=> |z| = 1)

H_z = h;  % because n_h goes from 0 to 4, so coefficients are ordered

order = n_h(end);
rs = roots(H_z);

% initialize cascade as a unit pulse
cascade = 1;
n_cascade = 0;
for i=1:length(rs)
   h_r = [1, -rs(i)];
   cascade = conv(h_r, cascade);  % chain of convolutions
   n_cascade = n_cascade(1) : n_cascade(end) + 1;  % extend support
end

cascade = h(1) * cascade;
y1 = conv(x, cascade);
n_y1 = n_x(1) + n_cascade(1) : n_x(end) + n_cascade(end);

figure(1);
stem(n_y, y);
hold on;
stem(n_y1, real(y1), '--');