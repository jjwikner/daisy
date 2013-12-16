function value = simDAC(option);

if (nargin == 0)
   % setup
   option = 'init';
end;

if (strcmp(option,'run'))
   output_current;
elseif (strcmp(option,'help'))
   figure(12);
   disp('Helptxt to be added!');
elseif (strcmp(option,'quit'));
   disp('nu skall allt stängas...');
   close all;
elseif (strcmp(option,'init'))
   close all;
   % Eventuellt en subrutin...
   simDACgui;   
elseif (strcmp(option,'default'))
   close all;
   % Eventuellt en subrutin...
   simDACgui;   
end;
