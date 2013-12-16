function [derivative] = daisyDerivate(inputSignal, fSample);
    
    derivative = fSample*[inputSignal(1) inputSignal(2:end)-inputSignal(1:(end-1))];
