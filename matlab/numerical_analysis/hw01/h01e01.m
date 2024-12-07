close all;
clc;
clearvars;

a = 1;
b = a;
while (a > 0)
   b = a;
   a = a / 2;
end
b
realmin