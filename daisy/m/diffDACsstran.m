function [output]=diffDACsstran(fs, X, Rp, Rn, Cp, Cn, Inom, Ri, Rsi, Ci, Vini)
% [output]=diffDACsstran(fs, X, Rp, Rn, Cp, Cn, Inom, Ri, Rsi, Ci, Vini)
% Transient analysis of differential current-steering DAC state-space model.
% X is the digital input signal. Vini (scalar) is the initial voltage value
% at the  downside of the current sources. Other parameters are the same as
% for DACss2, wich is used to simulate each sampling period. The output is
% given on differential form (2 rows, no. of columns = no. of samples).
%
% (c) Ola Andersson, MERC, Ericsson Microelectronics AB, July 2000
%

%set initial values of internal nodes
V=ones(1,length(Inom))*Vini;

%prepare empty output matrix
a=size(X);
output=zeros(2, a(1));
e=1;
d=1;

%simulate
for i=1:a(1)
  b=find(X(i,:)==1);
  c=find(X(i,:)==0);
  Yp=DACss2(fs, Inom(b), [V(b) d], Rp, Cp, Ri(b), Rsi(b), Ci(b));
  Yn=DACss2(fs, Inom(c), [V(c) e], Rn, Cn, Ri(c), Rsi(c), Ci(c));
%  disp(i);
  d=Yp(length(Yp));
  e=Yn(length(Yn));
  output(1,i)=d;
  output(2,i)=e;
  if length(Yp)>1
    V(b)=Yp(1:length(Yp)-1);
  end
  if length(Yn)>1
    V(c)=Yn(1:length(Yn)-1);
  end
end;



  
