function output = sampling(varargin)

% function output = sampling(varargin)
% 
% Function for sampling a time continous signal. For simplicity and
% accuracy this function has a built in signal generator.
% Non idealities such as kt/C noise and random clock jitter can be added.
%
% output.data is a vector containing the amplitude continous, time discrete
% data. output length is specified in input argument (default=8192)
% output.time is the uniform sampling instants
% output.jtime is the sampling instants if jitter was used.
%
% Input arguments are given in the form ('keyword',parameter)
%
% Keyword              Parameter   Description
%
% 'signal'             string      'sine' 'ramp' 'triangular' 'square'
% 'fin'                real        Input signal frequency
% 'fs'                 real        Sampling frequency
% 'Ain'                real        Input signal amplitude
% 'samples'            integer     Length of output vector
% 'jit_gaus'           real        Standard deviation for the gaussian clock
% jitter
% 'jit_sig'            real        Max signal dependent clock jitter for
% full scale signal in seconds
% 'Cs'                 real        Sampling capacitor size
% 'Rs'                 real        Sampling circuit resistance
% 'psd_in'             real        psd of input noise (assumed white)
% 'k2' 'k3' 'k4' 'k5'  real        Creates nonlineariy of different order
%
% Example:
%
% Revision history
% 050204 ES-LTH Created by Martin Anderson
% 050602 It shall be Vrms*randn() to get correct values ! /man
% 071202 Fixed a bug on HD found by Alberto Pugelli. Thanks Alberto! /man
% 080711 Added more comments /man
% 080711 Removed ns_in, Cs_in and Rs_in that was never used anyway /man
%

  tic;

% ----- Set default values ---------
  sig       = 'sine';   % signal type
  fin       = 9.97e6;   % sine frequency
  fs        = 81.92e6;  % sampling frequency
  samples   = 8192;     % nr of signal samples
  ain       = 1;        % input amplitude
  clkjit    = 0;        % jitter
  Cs        = inf;      % sampling capacitor size
  Rs        = inf;      % input resistance
  psd_in    = 0;        % input noise psd
  k2        = 0;        % second order nonlinearity coefficient
  k3        = 0;        % third order nonlinearity coefficient
  k4        = 0;        % fourth order nonlinearity coefficient
  k5        = 0;        % fifth order nonlinearity coefficient
  gjit      = 0;        % rms value of gaussian clock jitter
  sjit      = 0;        % ampltidue of signal dependent jitter [s]

  % ---- Set local variables, which could be changed by 'eval,'sentence' ----
  ntherm    = 0;
  kT        = 300*1.38066*1e-23;
  
% ----- Decode input parameters -----
  index = 1; 
  while index<=nargin
    switch lower(varargin{index})
     case {'signal' }
      sig = varargin{index+1};
      index=index+2;
     case {'fi' 'fin' 'finput' 'fsig'}
      fin = varargin{index+1};
      index=index+2;
     case {'fs' 'fsamp' 'fsample' 'sps'}
      fs = varargin{index+1};
      index=index+2;
     case {'ain' 'amp' 'amplitude'}
      ain = varargin{index+1};
      index=index+2;
     case {'jit_gaus'}
      gjit = varargin{index+1};
      index=index+2;
     case {'jit_sig'}
      sjit = varargin{index+1};
      index=index+2;      
     case {'cs'}
      Cs = varargin{index+1};
      index=index+2;
     case {'samples'}
      samples = varargin{index+1};
      index=index+2;
     case {'rs'}
      Rs = varargin{index+1};
      index=index+2;
     case {'psd_in' 'inputnoise_psd'}
      psd_in = varargin{index+1};
      index=index+2;  
     case {'k2'}
      k2 = varargin{index+1};
      index=index+2;         
     case {'k3'}
      k3 = varargin{index+1};
      index=index+2;     
     case {'k4'}
      k4 = varargin{index+1};
      index=index+2;     
     case {'k5'}
      k5 = varargin{index+1};
      index=index+2;
     otherwise
      warning(['---sampling.m --- Unknown input argument ' num2str(varargin{index})])
      index=index+1;
    end
  end
  
  % ----- Here starts the code -----

  % Create ideal and real sampling instants
  tideal = 1/fs*(0:(samples-1));
  treal = tideal + gjit*randn(size(tideal)) + sjit*sin(2*pi*fin*tideal); %Add clock jitter
   
  % Generate a time discrete signal sampled ideally and with the given jitter
  if (strcmp(sig,'sine'))
    x = ain*sin(2*pi*fin*treal);
    % Add nonlinear distortion
    x = x + k2.*x.^2 + k3.*x.^3 + k4.*x.^4 + k5.*x.^5;
  elseif 0
    % Here more signal types can be created if someone have the interest
    % for example ramps, multi-tone, square and triangular waves.
  else
    warning(['---sampling.m --- No valid signal type provided, generating' ...
	    ' sinusoid'])  
    x = ain*sin(2*pi*fin*treal);
    % Add nonlinear distortion
    x = x + k2.*x.^2 + k3.*x.^3 + k4.*x.^4 + k5.*x.^5;
  end
  
  % Generate and add sampling noise (kT/C) to the signal if specified
  % Also add first order filtered input noise from input noise PSD
  xn = x + psd_in/(4*Rs*Cs)*randn(size(x)) + sqrt(kT/(Cs))*randn(size(x));
      
  % ----- Return output signal and time vectors
  output.data  = xn;
  output.time  = tideal;
  output.jtime = treal;
  
  toc
 




