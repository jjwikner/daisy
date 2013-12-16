function s=daisyReserved(arg)

% MMBUILTIN Builtin Function Names. (MM)
% MMBUILTIN returns a cell array of strings containing the
% names of built in functions or reserved names in MATLAB.

% D.C. Hanselman, University of Maine, Orono ME 04469
% 3/25/00
% Mastering MATLAB 5, Prentice Hall, ISBN 0-13-858366-8
% www.eece.maine.edu/mm

s=cell(0);
mld = fullfile(matlabroot,'toolbox','matlab',''); % path to matlab subdirectory
d = dir(mld); % directory info in a structure
isdir = cat(1,d.isdir); % logical array pointing to directories
d = d(cat(1,d.isdir)); % take only those elements that are directories
[dirs{1:length(d)}] = deal(d.name); % convert directory names to a cell array
dirs(strncmp(dirs,'.',1))=[]; % throw out . and .. directories

for i=1:length(dirs) % loop over all directories found
   w = what(['matlab/' dirs{i}]) % structure of directory contents
   if length(w) > 0 
   for j = 1:length(w.m) % loop over M-files found
       mfile = w.m{j}(1:end-2); % grab j-th file, delete .m at end
       if exist(mfile,'builtin')==5 % if builtin add to list
           s=[s;{mfile}];
       end
   end
   end
end

s = unique(s); % eliminate duplicates and alphabetize