function x = sinusoid(t, A, f, phase)
    x = A * sin(2 * pi * f * t + phase);
end