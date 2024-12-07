clc
clear all
close all
A = [44 15 29 26 119;
15 33 32 18 15;
29 32 252 112 73;
26 18 112 124 90;
119 15 73 90 430];
b = [1 1 1 1 1]';

H = MyChol(A);  % or chol(A)'
y = H \ b;
x = H' \ y;  % equal to A \ b