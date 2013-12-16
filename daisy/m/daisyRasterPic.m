%
% User: jacobw
% Project name: daisy
% Project area: /site/edu/es/DAISY/daisy
%
% Department of Electrical Engineering
% Linkoping University 
%
% Mon Nov 14 09:38:08 CET 2011
%

A = imread('daisy/doc/figs/facebooklikebutton.png');
% Make it really b/w

A = 255*round(A/255);
% find(A>128)=255;
% find(A<=128)=0;

A = A(:,20:end,:);
A = A(1:(end-6),:,:);
% Rasterize
raster = 16;
A = A(1:raster:end,1:raster:end,:);

image(A)

pix = 1/80;

fid = fopen('daisy/skill/daisyRasterPicCmdFile.il','w');
fprintf(fid, 'cvId = (hiGetCurrentWindow)->cellView\n');

for y_line = 1:size(A,1)
    y = pix*(size(A,1)-y_line);
    for x_line = 1:size(A,2)   
        x = x_line*pix;
        if A(y_line,x_line,1)<=128
            fprintf(fid, ['(daisyLeRect cvId "text" (list (list %f %f) (list ' ...
            '%f %f)))\n'],x,y,x+pix,y+pix);
        end
    end
end
fclose(fid);
