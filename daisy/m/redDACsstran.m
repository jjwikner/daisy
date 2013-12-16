function [output]=redDACsstran(fs,X,Rp,Rn,Cp,Cn,Inom,Ri,Rsi,Ci,Vini,Vn) 
% [output]=diffDACsstran(fs, X, Rp, Rn, Cp, Cn, Inom, Ri, Rsi, Ci, Vini, Vn)
% Transient analysis of differential current-steering DAC state-space model.
% X is the digital input signal. Vini (scalar) is the initial voltage value
% at the  downside of the current sources. Other parameters are the same as
% for DACss2, wich is used to simulate each sampling period. The output is
% given on differential form (2 rows, no. of columns = no. of samples).
% Vn is the potential at the downside of Rn.
%
% (c) Ola Andersson, MERC, Ericsson Microelectronics AB, July 2000
%

%set initial values of internal nodes
V=ones(1,length(Inom))*Vini;
if length(Vn) < length(X)
  Vn=Vn(1)*ones(1,length(X));
end
%prepare empty output matrix
a=size(X);
output=zeros(2, a(1));
e=1;
d=1;

%simulate
for i=1:a(1)
  b=find(X(i,:)==1);
  c=find(X(i,:)==0);
  Yp=redDACss2(fs, Inom(b), [V(b) d], Rp, Cp, Ri(b), Rsi(b), Ci(b), 0);
  Yn=redDACss2(fs, Inom(c), [V(c) e], Rn, Cn, Ri(c), Rsi(c), Ci(c), Vn(i));
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



  
