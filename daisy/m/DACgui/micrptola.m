function micrptola(state); 

%
% micrptola.m
%
% Månadsrapport för MIC
%
% (c) jjwikner
%

global dstarth dendh dayflexh ackflexh hmnthrpt dstart dend homepath
global lstflexh

if (nargin == 0)
  state = 'init';
end;

lunch    = 0.6;
worktime = 38.75 / 5;
no_days  = 31;
month    = 'October';
holidays = zeros(1,no_days);
holidays([1 7 8 14 15 21 22 28 29]) = 1;
deltay   = 0.008;
ymargin  = 0.08;
xwdth    = 0.09;
yhgt     = [(1-2*ymargin) / (no_days-1) - deltay];
dayflex  = zeros(1,no_days);
ackflex  = zeros(1,no_days);
homepath = '/home/tde/olaa/timereports';
dayshift = 5;
day = {'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'};
if (strcmp('init',state))
  close all; hmnthrpt = figure(1);
  set(hmnthrpt,'NumberTitle','off', ...
      'Units', 'Normalized', ...
      'PaperUnits','centimeters', ...
      'PaperType','A4', ...
      'Name', month, ...
      'PaperPosition', [1.46644 1.41004 18.0485 26.8753], ...
      'Position', [0.05 0.05 0.5 0.93]);
  dstart   = 8.00*ones(1,no_days);
  dend     = 17.00*ones(1,no_days);
  lstflex  = 4.5;
  
  texth = [ putTxt([0.02 1-ymargin xwdth ymargin], 'Day'); ...
      putTxt([0.12 1-ymargin xwdth ymargin], 'Date'); ...
      putTxt([0.6 1-ymargin xwdth ymargin],'Begun'); ...
      putTxt([0.7 1-ymargin xwdth ymargin],'Went'); ... 
    putTxt([0.8 1-ymargin xwdth ymargin],'Sum Flex'); ...
    putTxt([0.9 1-ymargin xwdth ymargin],'Ack Flex'); ];

set(texth,'Backgroundcolor', [0.4 0.5 0.6], ...
    'HorizontalAlignment','center');

lstflexh = ...
    putFld([0.9 1-ymargin+yhgt xwdth yhgt], ...
    num2str(lstflex),'micrptola(''update'')', hmnthrpt);
sperx = 1-(ymargin + (no_days-1)*(yhgt+deltay/2));
urban = axes('position',[0 0 1 1]);
axis([0 1 0 1]); axis off;
for m = 1:no_days
  ypos = 1-(ymargin + (m-1)*(yhgt+deltay/2));
  turban = line([0 1],[1 1]*(ypos));
  set(turban,'LineStyle',':');
  daynmh(m) = ...
      putTxt([0.02 ypos xwdth yhgt], day{mod(m+dayshift,7)+1}, hmnthrpt);
      
  daynoh(m) = ...
      putTxt([0.12 ypos xwdth yhgt],num2str(m), hmnthrpt);
  
  if holidays(m)
    dstart(m) = 0;
    dend(m) = 0;
  end;
  
  dstarth(m) = ...
      putFld([0.6 ypos xwdth yhgt],...
      num2tme(dstart(m)),  ...
      'micrptola(''update'')', hmnthrpt);
  
  dendh(m) = ...
      putFld([0.7 ypos xwdth yhgt], ...
      num2tme(dend(m)), ...
      'micrptola(''update'')', hmnthrpt);
  
  if ~(holidays(m))
    dayflex(m) =  ...
	tme2num(get(dendh(m),'String')) - ...
	tme2num(get(dstarth(m),'String')) - ...
	- worktime - lunch;
  else
    dayflex(m) =  ...
	tme2num(get(dendh(m),'String')) - ...
	tme2num(get(dstarth(m),'String')) - ...
	- lunch;
  end;
  dayflexh(m) = ...
      putTxt([0.8 ypos xwdth yhgt], ...
      num2str(dayflex(m)), hmnthrpt);

  if (m > 1)
    ackflex(m) = ackflex(m-1) + dayflex(m);
  else
    ackflex(m) = dayflex(m) + str2num(get(lstflexh,'String'));
  end;
  
  ackflexh(m) = ...
      putTxt([0.9 ypos xwdth yhgt], ...
      num2str(ackflex(m)), hmnthrpt);
end;

putBtn([0.1 ymargin/2 0.15 yhgt],'Save data', 'micrptola(''save'')');
putBtn([0.3 ymargin/2 0.15 yhgt],'Load data', 'micrptola(''load'')');

set(daynoh(find(holidays)),'Backgroundcolor', [1.0 0.2 0.2]);
set(daynmh,'Backgroundcolor',[0.2 0.9 0.2]);
set(daynmh(find(holidays)),'Backgroundcolor', [1.0 0.2 0.2]);
set(dstarth(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(dendh(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(dayflexh(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(ackflexh(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);

micrptola('update');

elseif (strcmp('save',state))
  % Not implemented yet.
  fid = fopen([homepath, ...
	  '/month.report.',month,'.txt'],'w');
  for m = 1:no_days
    fwrite(fid, [num2str(m),' ']);
    if strcmp('', get(dstarth(m),'String'))
      fwrite(fid,['0 ']);
    else
      fwrite(fid, [get(dstarth(m),'String'),' ']);
    end;
    if strcmp('', get(dendh(m),'String'))
      fwrite(fid,['0 ']);
    else
      fwrite(fid, [get(dendh(m),'String'),' ']);
    end;
    fwrite(fid,' break ');
  end;
  fclose(fid);
elseif (strcmp('load',state))
  [dayno, starters, enders, breakers] = ...
      textread([homepath, ...
	  '/month.report.',month,'.txt'], ...
     '%s %s %s %s');
  if ~(length(dayno)==no_days);
    warning('Incompatible months!');
  end;
  for m = 1:no_days
    set(  dendh(m), 'String',   enders{m});
    set(dstarth(m), 'String', starters{m});
  end;
  micrptola('update');
  
elseif (strcmp('update',state))
  for m = 1:no_days
    %    set(dstarth(m),'String',num2tme(dstart(m)));
    %    set(dendh(m),'String',num2tme(dend(m)));
    if ~(holidays(m))
      dayflex(m) = ...
	  tme2num(get(dendh(m),'String')) - ...
	  tme2num(get(dstarth(m),'String')) - ...
	  worktime - lunch;
      if strcmp('', get(dendh(m),'String')) & ...
	    strcmp('', get(dstarth(m),'String'))
	dayflex(m) = dayflex(m) + lunch;
      end;
    else
      dayflex(m) = ...
	  tme2num(get(dendh(m),'String')) - ...
	  tme2num(get(dstarth(m),'String')) - lunch;
      if (tme2num(get(dendh(m),'String'))==0) & ...
	    (tme2num(get(dstarth(m),'String'))==0) 
	dayflex(m) = dayflex(m) + lunch;
      end;
    end;
    set(dayflexh(m),'String', num2str(0.01*round(100*dayflex(m))));
    if (m > 1)
      ackflex(m) = ackflex(m-1) + dayflex(m);
    else
      ackflex(m) = dayflex(m) + str2num(get(lstflexh,'String'));
    end;
    set(ackflexh(m),'String', num2str(0.01*round(100*ackflex(m))));
  end;
end;
    

