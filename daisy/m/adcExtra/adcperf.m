function output = adcperf(varargin);

% function output = adcperf(varargin);
%
% Analyzes a frequency spectrum created by the function
% adcfft.m
%
% Outputs a vector with the desired performance measures
%
% 'data'    value: Input data vector.
% 'osr'     value: Oversampling ratio for input data
% 'plots'   value: 1=output all plots.
% 'snr'            Calculate snr 
% 'sndr'           Calculate sndr
% 'sfdr'           Calculate sfdr
% 'sdr'            Calculate sdr
% 'win' 'w'        Windowing function used in spectrum
% 'D'              Intended decimation factor
% 'sigbins'        Specification of signal bins (used in multi-tone evaluation)
% 'nodist'         Flag to indicate that no distortion shall be estimated. SNR=SNDR
%
% EXAMPLE:
%
% Revision history:
% 040601 - Created by Martin Anderson, ES-LTH.
% 040810 - Added windowing, comments and varargin - MAN
% 050629 - Added hann2 window compensation - MAN
% 050702 - Changed name and rewrote the script to handle oversampled
% systems - MAN
% 050703 - Added functionality to handle signal leakage - MAN
% 050707 - Added pre-decimation SNR calculation for DF design support- MAN
% 050919 - Added functionality to evaluate multi-tone inputs - MAN
% 050920 - Added possibility to turn off distortion estimation - MAN
% 081107 - Moved window attenuation compensation from adcfft to adcperf -
% MAN
% 081107 - Changed from boxcar to rectwin /man
%
% (c) 2004 - Martin Anderson, ES-LTH.
%     For educational and research purposes only. 
%     All other use is strictly prohibited.
%

plots = 0; % No plots by default
snr = 0;
osr=1;
sndr = 0;
D = 1;
sdr = 0;
sfdr = 0; % Dont do unnecessary calculations
allcalc = 0;
nb=1; %Default is boxcar
nc=1;
sigbins = [];
sigbinin=0;
nodist = 0;
wcomp=1; % rectwin default

% Analyze input arguments
index = 1;
while index <= nargin    
    switch (lower(varargin{index}))
    case {'data' 'spec' 'indata' 's' 'fft' 'spectrum'}
        u = varargin{index+1};
        index = index+2; 
     case {'osr'}
        osr = varargin{index+1};
        index = index+2;     
    case {'sigbins'}
        sigbins = varargin{index+1};
        sigbinin = 1;
        index = index+2;
    case {'plots' 'plot' 'fig' 'figs'}
        plots=1;
        index = index+1;
    case {'snr' 'noise'}
        snr=1;
        index = index+1;    
     case {'d' 'dec' 'decimate' 'df' 'd1' 'd2' 'd3' 'd4'}
        D=varargin{index+1};
        index = index+1;
    case {'sndr' 'sinad'}
        sndr=1;
        index = index+1;
    case {'sfdr'}
        sfdr=1;
        index = index+1;         
    case {'nodist'}
        nodist=1;
        index = index+1;     
    case {'sdr' 'distortion' 'dist'}
        sdr=1;
        index = index+1;       
     case {'win' 'w' 'window'}        
      if (strcmp(varargin{index+1},{'boxcar' 'rect' 'rectwin'})) 
            nb = 1;
            nc = 1;
            wcomp = 1;
      elseif (strcmp(varargin{index+1},'hann1')) 
            nb = 3;
            nc = 1.5;
            wcomp = 2;
      elseif (strcmp(varargin{index+1},'hann2')) 
            nb = 5;
            nc = 35/18;
            wcomp = 8/3;
      end
        index = index+2;  
    otherwise
        index=index+1;
    end
end

% Compensate for window attenuation
u = u*wcomp;

% Locate signal band
if plots==1
  subplot(211); hold on;
  plot(20*log10(abs(u)),'k-')
end
inbandbins = 1:floor(length(u)/(osr)); 
% For dec. part:
foldlen  = floor(length(u)/(osr)); lu=length(u); d=u;
u = abs(u(inbandbins));
if plots == 1
  subplot(212); hold on;
  plot(20*log10(abs(u)),'k-')
  plot(20*log10(abs(u)),'k.')
end

% Find all peaks
a = find((u(1:length(u)-1)-u(2:length(u)))>0);
b = find((u(2:length(u)-1)-u(1:length(u)-2))>0)+1;
pbins = intersect(a,b);

% Plot peaks
if plots == 1
  subplot(211); hold on;
  plot(pbins,20*log10(abs(u(pbins))),'y.');
end

% Find the signal peak(s) (this requires the signal to be the strongest tone
% inband or specified by sigbins)
if sigbinin == 0
  sbin = find(u==max(u(pbins)));
  sbins = sbin + [-(nb-1)/2 : (nb-1)/2];
   if plots == 1
    subplot(211); hold on;
    plot(sbins,20*log10(abs(u(sbins))),'g.');
    plot(sbin,20*log10(abs(u(sbin))),'go');  
    subplot(212); hold on;
    plot(sbins,20*log10(abs(u(sbins))),'g.');
    plot(sbin,20*log10(abs(u(sbin))),'go');
   end
  pbins=setdiff(pbins,sbins);
  inbandbins=setdiff(inbandbins,sbins);
elseif sigbinin == 1
  sbin = sigbins+1;
  for m = 1:length(sbin)
    sbins(nb*m-nb+1:nb*m) = sbin(m) + [-(nb-1)/2 : (nb-1)/2];
  end
  sbins = unique(sbins);   
  if plots == 1
    subplot(211); hold on;
    plot(sbins,20*log10(abs(u(sbins))),'g.');
    plot(sbin,20*log10(abs(u(sbin))),'go');  
    subplot(212); hold on;
    plot(sbins,20*log10(abs(u(sbins))),'g.');
    plot(sbin,20*log10(abs(u(sbin))),'go');
   end
  pbins=setdiff(pbins,sbins);
  inbandbins=setdiff(inbandbins,sbins);
end

% Estimate which bins that are distortion peaks
% if nodist = 0
dbins=[]; dbin=[];
if nodist == 0
  med = median(u(pbins));
  mi = min(u(inbandbins));
  %ma = med + (med - mi); 
  ma = 1.6*med;
  dbin = pbins( find( u(pbins) > ma ) );
  if plots == 1
    subplot(211); hold on;
    plot(dbin,db20(abs(u(dbin))),'ro');
  end
% Remove distortion leakage from noise bins
  for i = 1:length(dbin)
    dbins = [dbins (dbin(i) + [-(nb-1)/2 : (nb-1)/2])];
  end
  dbins=unique(dbins);
  if plots == 1
    subplot(211); hold on;
    plot(dbins, db20(abs(u(dbins))),'m.');
  end
  inbandbins = setdiff(inbandbins,dbins);
  inbandbins = inbandbins(4:length(inbandbins)); % Remove three dc bins
end

% Calculate signal and noise powers
sig = sum(u(sbin).*u(sbin));  
noise = sum(u(inbandbins) .* u(inbandbins))/nc;
avgnoise = noise / length(inbandbins);
noise = avgnoise*(length(inbandbins)+length(sbins)+length(dbins));
length(inbandbins)+length(sbins)+length(dbins); % UNSOLVED::: Why not the same in all simulations ???
dist = sum(u(dbin) .* u(dbin));

% Calculate performances
if noise > 0
  output.snr  = 10*log10(sig/noise);
else
  output.snr = inf;
end
if dist > 0
  output.sdr  = 10*log10(sig/dist);
else
  output.sdr = inf;
end
if (dist+noise > 0)
  output.sndr = 10*log10(sig/(dist+noise));
else
  output.sndr = inf;
end
if length(dbin > 0) 
  output.sfdr = 10*log10(sig/(max(u(dbin)).*max(u(dbin))));
else
  output.sfdr = 10*log10(sig/(max(u(inbandbins).*max(u(inbandbins)))));
end

% Consider all folding bands, if D > 1
if D > 1
 foldbins=[];
 foldcenters = floor(2/D*lu)*[1:1:D];
 for k = 1:length(foldcenters) 
   for j = 1:(2*foldlen)
     foldbins = [foldbins foldcenters(k)-foldlen+j];     
   end
 end
 foldbins = foldbins(find(foldbins<length(d)));
 if plots == 1
  subplot(211); hold on;
  plot(foldbins, 20*log10(abs(d(foldbins))), 'r.');
 end
 foldnoise = sum(d(foldbins).*d(foldbins))/nc;
 output.d_sndr = 10*log10(sig/(dist+noise+foldnoise));
 output.degradation = output.d_sndr - output.sndr;
 output.ifnr = 10*log10(noise/(foldnoise));
end

% The programmers guide to adcperf.m:
% sbin = the signal peak bins
% sbins = peaks + window leakage bins
% dbin = the distortion peak bins
% dbins = the distortion bins + window leakage
% inbandbins = all bins - distortion bins - signal bins - leakage bins
