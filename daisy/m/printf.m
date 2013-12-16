% function s = printf(str, args)
% function s = printf(ptr, str, args)
%  Like fprintf, but better.
%  * ptr = vector with filepointers, except 0 = no output (gives error with fprintf). Default = [1], or [] if an output is required.
%    ptr might be a cell array, with file pointers or names. If a name is given, it is opened in append mode.
%  * str, args = format string and arguments, just like fprintf, with one exception:
%      Argument might be a cell array with the format {(fmt, (del)), arg}, which will print a list.
%      * fmt = optional format for the values in arg. Mandantory if del is given.
%      * del = optinal delimiter between the values. Default = ", ".
%      * arg = matrix/cell array.
%      If arg is a matrix and fmt contains '%s', then each line in arg is conciderred a string.
%      If arg is a matrix, and fmt does NOT contain '%s', then all elements in arg is taken in natural order (row wise).
%      For this case, use "%s" in str. In case fmt contains "%s" and arg is a character matrix, each "value" will be a row in arg.
%  * s = printed string, returned.
%
% Ex:
%   printf(0,'hi')
%    => (nothing happens).
%   printf([1 2],'hi\n')
%    => print 'hi' on stdout and stderr.
%   printf({fp,1},'len(v)=%d, v=[%s];\n', length(v), {'%d', v})
%    => print 'len(v)=3, v=[3, 9, 5]' to file pointer fp, and on stdout.
%   s = printf({1,'rtl.vhd'}, ...         % Print to stdout, append to file rtl.vhd and save in variable s.
%              '  y(%d) <= %s;\n', ...    % The str: Main format string.
%              i, ...                     % Maps to '%d' in str.
%              {'x(%d)', ' OR ', v});     % fmt='x(%d)', del=' OR', arg=v=[3 5 9]. Maps to %s in str.
%    => Generate the string '  y(5) <= x(3) OR x(9) OR x(5);\n'.
%    => Print it on stdout, append it to rtl.vhd, and return it to variable s.
% 
% OBS.
% * printf is NOT optimized for speed. Long lists might slow down the function markedly.
% * printf is NOT idiot safe - if you input any wrong format it might crash.

% Petter Källström

function s = printf(varargin)
  if ~nargin, return; end
  args = varargin;
  if ischar(args{1})
    ptrs = 1;
    str = args{1};
    args = args(2:end);
  else
    ptrs = args{1};
    str = args{2};
    args = args(3:end);
  end
  
  % Find cells in argv
  for i=1:length(args)
    if iscell(args{i})
      c = args{i};
      if length(c) == 1
        args{i} = v2s(c{1},', ',NaN);
      elseif length(c) == 2
        args{i} = v2s(c{2},', ',c{1});
      else
        args{i} = v2s(c{3},c{2},c{1});
      end
    end
  end
  
  % print
  for i=1:length(ptrs)
    if iscell(ptrs)
      if ischar(ptrs{i})
        fp = fopen(ptrs{i},'a');
        fprintf(fp,str,args{:});
        fclose(fp);
      elseif ptrs{i}
        fprintf(ptrs{i},str,args{:});
      end
    elseif ptrs(i)
      fprintf(ptrs(i),str,args{:});
    end
  end
  if nargout
    s = sprintf(str,args{:});
  end
end

function s = v2s(v, del, fmt)
% Vector/matrix/cellarray to string function.
% v: column vector/matrix/cellarray with values to be printed.
  if     iscell(v)
    s = cell2s(v,del,fmt);
  elseif (~ischar(fmt) && ischar(v)) || (ischar(fmt) && ~isempty(strfind(fmt,'%s')))
    s = cmat2s(v,del,fmt);
  else
    s = vec2s(v,del,fmt);
  end
end

function s = vec2s(v, del, fmt)
  if isnan(fmt), fmt = '%d'; end
  if ~isempty(v)
    s = sprintf(fmt,v(1));
    for i=2:numel(v)
      s = [s del sprintf(fmt, v(i))]; %#ok<AGROW>
    end
  end
end

function s = cmat2s(v, del, fmt)
  if isnan(fmt), fmt = '%s'; end
  if size(v,1)>0
    s = sprintf(fmt,v(1,:));
    for i=2:size(v,1)
      s = [s del sprintf(fmt, v(i,:))]; %#ok<AGROW>
    end
  end
end

function s = cell2s(v, del, fmt)
  if isnan(fmt), fmt = '%s'; end
  if ~isempty(v)
    s = sprintf(fmt,v{1});
    for i=2:numel(v)
      s = [s del sprintf(fmt, v{i})]; %#ok<AGROW>
    end
  end
end
