function str = acklamLatex(mat, format)
%LATEXMAT Generate LaTeX code for a matrix.
%
%   STR = LATEXMAT(MAT, FORMAT) return the LaTeX code for the matrix MAT, with
%   the given FORMAT.
%
%   See HELP SPRINTF for more details about the FORMAT parameter.

%   Author:      Peter John Acklam
%   Time-stamp:  2003-07-12 22:14:14 +0200
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

error(nargchk(2, 2, nargin)) ;
if ischar(mat)
    error('First argument can not be a string.') ;
end
if ~ischar(format)
    error('Second argument must be a string.') ;
end
[ r, c ] = size(mat) ; newline = sprintf('\n') ;

str = [ '\begin{table}' newline ...
        '\caption{}' newline ...
        '\centering' newline ...
        '\begin{tabular}{'] ;       
for cols = 1:c
    str = [str '|',char(abs('c'))] ;       
end
str = [str '|}' newline '\hline' ] ; 
              
for i = 1:r
    str = [ str ' ' ] ;
    for j = 1:c
        t = sprintf(format, real(mat(i,j))) ;
        if (imag(mat(i,j)) > 0)
            t = [ t '+' sprintf(format, imag(mat(i,j))) 'i' ] ;
        elseif (imag(mat(i,j)) < 0)
            t = [ t '-' sprintf(format, -imag(mat(i,j))) 'i' ] ;
        end        
        str = [ str ' $',t,'$' ] ;        
        if j < c
            str = [ str ' &' ] ;
        else            
            if i < r
                str = [ str ' \\ ' newline ] ;
                str = [ str ' \hline ' newline ] ;              
            else                
                str = [ str ' \\ ' newline ] ;
                str = [ str ' \hline' newline ] ;
            end            
        end        
    end     
end
str = [ str '\end{tabular}' newline ...
            '\label{}' newline ...
            '\end{table}'] ; 