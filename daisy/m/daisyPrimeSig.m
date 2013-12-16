function fcoh = daisyPrimeSig(fSignal, fSample, numberOfSamples);
%
% function fcoh = daisyPrimeSig(fSignal, fSample, numberOfSamples);
%
% fSignal : target signal frequency
% fSample : sample frequency
% numberOfSamples : number of samples in the vector
%
% fcoh    : output signal frequency fulfilling prime coherent sampling.
%
% Notice that the function might return an empty matrix which means that
% no prime number was found.
%
% J Jacob Wikner, Linköping University
%
    
    numberOfCycles = max(primes(numberOfSamples*fSignal/fSample));
    % New approach (take the prime numbers in the whole vector)
    rr = primes(round( ceil(fSignal/fSample) *  numberOfSamples));
    pp = (rr - fSignal/fSample*numberOfSamples).^2;
    fcoh = rr( find( min(pp)== pp )   )*fSample/numberOfSamples;
    
    % fcoh = numberOfCycles*fSample/numberOfSamples;
