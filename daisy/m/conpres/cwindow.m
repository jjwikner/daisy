function windowedvalues = cwindow(kvalues, windowtype, param);

%WINDOWNAMES =	Rectangular Hamming Hanning Triangular Bartlett 
%		Gaussian Blackman Kaiser Chebyshev

if (windowtype == 2);
	windowedvalues = kvalues.*hamming(size(kvalues,2))';
elseif (windowtype == 3)
	windowedvalues = kvalues.*hanning(size(kvalues,2))';
elseif (windowtype == 4)
	windowedvalues = kvalues.*triang(size(kvalues,2))';
elseif (windowtype == 5)
	windowedvalues = kvalues.*bartlett(size(kvalues,2))';
elseif (windowtype == 6)

elseif (windowtype == 7)
	windowedvalues = kvalues.*blackman(size(kvalues,2))';
elseif (windowtype == 8)
	windowedvalues = kvalues.*kaiser(size(kvalues,2),param)';
elseif (windowtype == 9)
	win = chebwin(size(kvalues,2),param)';
	if (size(win) == size(kvalues))
		windowedvalues = kvalues.*win;
	else
		windowedvalues = kvalues.*win(1:size(kvalues,2));
	end;
else
	windowedvalues = kvalues;
end;



