function [result] = z2gamma(z)
%Z2GAMMA Convert impedance to reflection coefficient.
%        GAMMA = Z2GAMMA(Z) converts the impedances in matrix Z to
%        reflection coefficients in GAMMA. Z should be normalized 
%        with respect to the characteristic impedance Z0.
 
result = (z-1)./(z+1);
