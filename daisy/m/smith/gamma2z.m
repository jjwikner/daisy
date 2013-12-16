function [z] = gamma2z(gamma)
%GAMMA2Z Convert reflection coefficient to impedance.
%        Z = GAMMA2Z(GAMMA) converts the reflection coefficients
%        in matrix GAMMA to impedances in Z. The Z values are
%        normalised with respect to the characteristic impedance Z0.
 
z = (1+gamma)./(1-gamma);
