function [MenuOptions, MenuActions] = cMenuContents

% Menuoptions to the conpres program

% MenuOptions

idealfuncoptions = str2mat(		...
	'>&Ideal function',		...
	'>>&Sine', ...
	'>>&Ramp', ...
	'>>&Pulse Return to zero', ...
	'>>&Pulse alternating', ...
	'>>&Sawtooth', ...
	'>>&Triangular');


loadmenuoptions = str2mat(		...
	'>&Load new file',		...
	'>>Load &Spice file ...',	...
	'>>Load &Digital data ...',	...
	'>>Load &Analog data ...');

savemenuoptions = str2mat(			...
	'>&Save displayed data',		...
	'>>Save as &analog information ...',	...
	'>>Save as &digital information ...');

printmenuoptions = str2mat( ...
	'>&Print figure ...');

filemenuoptions = str2mat( ...
	'&File', ...
	loadmenuoptions, ...
	idealfuncoptions, ...
	savemenuoptions, ...
	'>-------', ...
	printmenuoptions, ...
	'>-------', ...
	'>&Quit^q');

Timedomainoptions = str2mat( ...
	'>&Time Domain', ...
	'>>&Time Plot', ...
	'>>&Modulo Time Plot');

Frequencydomainoptions = str2mat( ...
	'>&Frequency Domain', ...
	'>>&FFT', ...
	'>>&LogFFT', ...
	'>>&Bode Plot');

Statisticaloptions = str2mat( ...
	'>&Statistical', ...
	'>>&Histogram', ...
	'>>&DNL', ...
	'>>&INL');

Dynamicoptions = str2mat( ...
	'>&Dynamic', ...
	'>>&DNL', ...
	'>>&INL', ...
	'>>&Correlation');
		
viewmenuoptions = str2mat( ...
	'&View Type', ...
	Timedomainoptions, ...
	Frequencydomainoptions, ...
	Statisticaloptions, ...
	Dynamicoptions);

functionmenuoptions = str2mat( ...
	'&Function Type', ...
	'>&Sine', ...
	'>&Ramp', ...
	'>&Pulse Return to zero', ...
	'>&Pulse alternating', ...
	'>&Sawtooth', ...
	'>&Triangular' ...
	);

windowtypes = str2mat(		...
	'>&Windowing',		...
	'>>&Rectangular',	...
	'>>&Hamming',		...
	'>>H&anning',		...
	'>>&Triangular',	...
	'>>&Bartlett',		...
	'>>&Gaussian',		...
	'>>B&lackman',		...
	'>>&Kaiser',		...
	'>>&Chebyshev');
	
algorithmmenuoptions = str2mat( ...
	'&Algorithms',		...
	windowtypes,		...
	'>Wa&velets',		...
	'>&Compute Parameters');
	
Graphmenuoptions = str2mat( ...
	'&Graph Options', ...
	'>Toggle &grid', ...
	'>&Zooming', ...
	'>>Toggle &Zoom', ...
	'>>Zoom &out' ,...
	'>Toggle &background');

helpmenuoptions = str2mat( ...
	'&Help', ...
	'>&Help on contents', ...
	'>&About Conpres');

MenuOptions = str2mat(...
	filemenuoptions, ...
	viewmenuoptions, ...
	algorithmmenuoptions, ...
	functionmenuoptions, ...
	Graphmenuoptions, ...
	'', ...
	helpmenuoptions);

% **************************************************************
% ********** MenuActions
% ****************************

idealfuncactions = str2mat(			...
	'',					...
	'cinterface(''createidealdata'',1);',	...
	'cinterface(''createidealdata'',2);',	...
	'cinterface(''createidealdata'',5);',	...
	'cinterface(''createidealdata'',6);',	...
	'cinterface(''createidealdata'',3);',	...
	'cinterface(''createidealdata'',4);');

loadmenuactions = str2mat( ...
	'', ...
	'cinterface(''loadnewfile'',''S'')', ...
	'cinterface(''loadnewfile'',''D'')', ...
	'cinterface(''loadnewfile'',''A'')');

savemenuactions = str2mat( ...
	'', ...
	'cinterface(''savefile'', ''A'')', ...
	'cinterface(''savefile'', ''D'')');

printmenuactions = str2mat( ...
	'printdlg(1,[.13 .11 .62 .815],get(gcf,''defaultaxesposition''))');

filemenuactions = str2mat(	...
	'',			...
	loadmenuactions,	...
	idealfuncactions,	...
	savemenuactions,	...
	'',			...
	printmenuactions,	...
	'',			...
	'cinterface(''done'')');

Timedomainactions = str2mat( ...
	'', ...
	'cinterface(''viewtype'',1)', ...
	'cinterface(''viewtype'',7)');

Frequencydomainactions = str2mat( ...
	'', ...
	'cinterface(''viewtype'',2)', ...
	'cinterface(''viewtype'',3)', ...
	'');

Statisticalactions = str2mat( ...
	'', ...
	'cinterface(''viewtype'',4)', ...
	'cinterface(''viewtype'',5)', ...
	'cinterface(''viewtype'',6)');

Dynamicactions = str2mat( ...
	'', ...
	'cinterface(''viewtype'',9)', ...
	'cinterface(''viewtype'',10)', ...
	'cinterface(''viewtype'',8)');

viewmenuactions = str2mat( ...
	'', ...
	Timedomainactions, ...
	Frequencydomainactions, ...
	Statisticalactions, ...
	Dynamicactions);

functionmenuactions = str2mat( ...
	'', ...
	'cinterface(''change_functype'',1)', ...
	'cinterface(''change_functype'',2)', ...
	'cinterface(''change_functype'',5)', ...
	'cinterface(''change_functype'',6)', ...
	'cinterface(''change_functype'',3)', ...
	'cinterface(''change_functype'',4)' ...
	);
	
windowacts = str2mat(				...
	'',					...
	'cinterface(''change_window'',1)',	...
	'cinterface(''change_window'',2)',	...	
	'cinterface(''change_window'',3)',	...
	'cinterface(''change_window'',4)',	...
	'cinterface(''change_window'',5)',	...
	'cinterface(''change_window'',6)',	...
	'cinterface(''change_window'',7)',	...
	'cinterface(''change_window'',8)',	...
	'cinterface(''change_window'',9)');

algorithmmenuactions = str2mat(		...
	'',				...	
	windowacts,			...
	'',				...
	'cinterface(''compute_parameters'')');

Graphmenuactions = str2mat( ...
	'', ...
	'grid', ...
	'', ...
	'cinterface(''zooming'',''toggle'')', ...
	'cinterface(''zooming'',''zoomout'')', ...
	'whitebg;');

helpmenuactions = str2mat( ...
	'', ...
	'cinterface(''help'');', ...
	'cinterface(''info'');');
		
MenuActions = str2mat( ...
	filemenuactions, ...
	viewmenuactions, ...
	algorithmmenuactions, ...
	functionmenuactions, ...
	Graphmenuactions, ...
	'', ...
	helpmenuactions);
