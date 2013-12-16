function output = quantization(varargin)

% function output = quantization(varargin)
% 
% Function for quantizing the vector x into 2^R discrete levels. The
% function also has options to add random noise and nonlinearity in the
% quantization process.
% 
% output is a vector of the same length as the input vector x.
%
% Input arguments are given in the form ('keyword',parameter)
%
% Keyword              Parameter   Description
%
% 'data'               vector      Vector with input data on decimal form
% 'N' 'R' 'bits' 'res' integer     Defines the resolution of the quantization
% 'npow'               real        Defines the random noise level
% 'vref'               real        Bipolar reference level = maximum quantized signal; 
%                                  minimum reference level is assumed to be zero.
%
% Example: (Performs a 10 bit ideal quantization of the 1 kHz input sinusoidal)
% >> x = sin(2*pi*1000 * [0:1:1024]);
% >> quantization('data',x,'R',10, 'Vref', 1)
%
% Revision history
% 050202 ES-LTH Martin Anderson
% 050302 implemented clipping /man
% 081107 implemented 'vref' and deleted 'ain' /piero
% 081107 cleaned up script and added comments /man

  tic;

% ----- Set default values ---------
  R         = 10;   % Default resolution
  data      = [];   % Amplitude continuouos input data
  npow      = 0;    % Power of additive gaussian noise
  Vref      = 2;    % Default Vref = 2 because in this way the highest acceptable 
                    % peak voltage for ain (sinusoid) is 1
 
% ----- Decode input parameters -----
  index = 1; 
  while index<=nargin
    switch lower(varargin{index})
     case {'data' }
      data = varargin{index+1};
      index=index+2;
      datainput = 1;
     case {'r' 'n' 'bits' 'bit' 'res'}
      R = varargin{index+1};
      index=index+2;
     case {'npow'}
      npow = varargin{index+1};
      index=index+2;
     case {'vref' 'Vref'}
      Vref = 2*varargin{index+1};
      index=index+2;  
     otherwise
      warning(['unknown input argument ' varargin{index}])
      index=index+1;
    end
  end
  
 % ----- Here starts the code -----
  
  data = data + sqrt(npow)*randn(size(data)); % Add random noise
							       
  data = data + Vref/2;               % DC shift - data is now between 0 and Vref
  data = data*((2^R)/Vref);           % This normalizes the quantization error to +- 1/2,
                                      % and the quantized signal can be
                                      % recovered by rounding the signal to
                                      % the closest integer.
 
  data = floor(data);                 % Round to closest lowest level 

  data(find(data >= (2^R))) = 2^R-1;  % Remove highest level and
  data(find(data<0)) = 0;             % implement clipping
  data = data/((2^R)/Vref);           % Denormalized
  output = data-Vref/2;
				 
% Print time
  toc
 