% INLDNL(filename, R, strobe)
%
% This is a program to evaluate an ADC output from a ramp input
% The ratio of the number of samples taken and the number of q-levels
% determines the resolution of the INL/DNL plots. The resolution is 
% qlevels/samples LSBs.
%
% filenmae :    the full path and name of your data file
% R :           the resolution of the ADC
% strobe :      the number of samples per qlevel
%
% EXAMPLE:
% a=INLDNL('samples.dat',8,10);
%
% Created by Martin Anderson, LTH, (c)2004.
% For educational and private use only.
% Last edit by MAN 20040805


function output = INLDNL(filename, R, strobe);

Vfs = 1;

data = load(filename);
V1 = data(:,1);
V1data = (round(V1*10000))/10000;


qlevels = 2^R-1;
samples = qlevels * strobe;

% -- Remove unnecessary samples
%i=1;
%old=V1(1);
%new=V1(1);
%while 1 
%    if (old == new)
%        i=i+1;
%       old=new;
%        new = V1(i);
%        
%   else
%        V1data = V1(i-strobe:i+samples-strobe-1);
 %       break;
 %  end
 %end
%    V1data = V1(1:i+samples-1);


% -- Calculate DNLs
j=1;
k=0;
m=0;

old = V1data(j);
new = V1data(j);

for j = 1:length(V1data)
    new = V1data(j);
    if (old == new)
        k=k+1;     
    end
    if (old ~= new)      
        m=m+1;
        DNL(m) = (k-strobe)/strobe;
        old = V1data(j) ;
        k=1;     
    end
end
       
% Calculate the INL from the DNLs
INL(1:length(DNL))=0;
for k=1:length(DNL)
    for j=1:k
        INL(k)=INL(k)+DNL(j);
    end
end

% Do some plotting
ah = axes;

th = title('INL / DNL with gain error');
xh = xlabel('Normalized input level');
yh = ylabel('INL / DNL [LSBs]');

set(ah, 'FontSize', 16);
set(th, 'FontSize', 16);
set(xh, 'FontSize', 16);
set(yh, 'FontSize', 16);
axis([0.0,1,-0.7,2.3]);


hold on;
%grid;

x=1:length(DNL);
x=x/length(DNL);

hold on;
%plot(x,DNL,'k-')
plot(x,INL,'b-')

output=1;