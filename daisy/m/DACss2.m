function [output]=DACss2(fs, Inom, Vini, R, Co, Ri, Rsi, Ci)

% [output]=DACss2(fs, Inom, Vini, R, Co, Ri, Rsi, Ci)
% Creates a state-space model of one output channel of a current-steering
% DAC and simulates one switching period. fs is the sampling frequency, Inom
% is a vector containing the nominal current values of the current sources
% connected to, Ri, Rsi and Ci are vectors containing the corresponding
% output resistances, switch resistances and output capacitances
% respectively. R and C are the load resistance and capacitance
% respectively, including wire parasitics. 
% Vini is a vector containing the initial values of the internal nodes of
% the current sources and the initial output (last element). 
% The output is given as [Vfin Vo], where Vo is the (final) output voltage
% (scalar), and Vfin is a vector containing the final values of the internal
% node voltages.
%
% (c) Ola Andersson, MERC, Ericsson Microelectronics AB, July 2000
%

% construct the state matrices
if length(Vini)==1
  A=[-1/R/Co];
  B=0;
  C=[1];
  D=Vini;
else
  A=(1./Ri+1./Rsi).*(1./Ci);
  A=-diag(A);
  a=1./(Rsi.*Ci);
  A=[A a'];
  a=1./(Co*Rsi);
  b=-(ones(1,length(Rsi))*(1./Rsi)'+1/R)/Co;
  a=[a b];
  A=[A;a];
  B=[diag(1./Ci)*Inom';0];
end

% simulate with a unit step input for 1/fs s
T=1/fs;
output=expm(A*T)*Vini'+A\(expm(A*T)*B-B);





