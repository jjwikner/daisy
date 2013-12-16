function micrpt(state); 

%
% micrpt.m
%
% Månadsrapport för MIC
%
% (c) jjwikner
%

global dstart2h dend2h dayflexh ackflexh hmnthrpt
global dstart1h dend1h totackh totwrkh
global lstflexh dlunchh dcodeh

if (nargin == 0)
  state = 'init';
end;

lunch    = 0.6;
worktime = 38.75 / 5;
no_days  = 30;
month    = 'November';
holidays = zeros(1,no_days);
holidays([4 5 11 12 18 19 25 26]) = 1;
deltay   = 0.008;
ymargin  = 0.1;
xwdth    = 0.08;
xmargin  = 0.02;
xm = xmargin;
ym = ymargin;
xs = 1.1;
yhgt     = [(1-2*ymargin) / (no_days-1) - deltay];
dayflex  = zeros(1,no_days);
ackflex  = zeros(1,no_days);
homepath = [getenv('HOME') '/'];
dayshift = 1;
day = {'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'};

if (strcmp('init',state))
  close all; hmnthrpt = figure(1);
  set(hmnthrpt,'NumberTitle','off', ...
      'Units', 'Normalized', ...
      'PaperUnits','centimeters', ...
      'PaperType','A4', ...
      'Name', month, ...
      'PaperPosition', [1.46644 1.41004 18.0485 26.8753], ...
      'Position', [0.05 0.05 0.6 0.93]);
  dstart1   = 8.00*ones(1,no_days);
  dend1     = 17.00*ones(1,no_days);
  dstart2   = zeros(1,no_days);
  dend2     = zeros(1,no_days);

  dcode     = zeros(1,no_days);
  lstflex  = 4.5;

  % Text at top of screen
  
  texth = [ putTxt([xm 1-ym xwdth ym], 'Day'); ...
      putTxt([xm+xwdth*xs 1-ym xwdth ym], 'Date'); ...
      putTxt([xm+2*xwdth*xs 1-ym xwdth ym],'Code'); ...
      putTxt([xm+3*xwdth*xs 1-ym xwdth ym],'Begun'); ...
      putTxt([xm+4*xwdth*xs 1-ym xwdth ym],'Went'); ... 
      putTxt([xm+5*xwdth*xs 1-ym xwdth ym],'Begun'); ...
      putTxt([xm+6*xwdth*xs 1-ym xwdth ym],'Went'); ... 
    putTxt([xm+7*xwdth*xs 1-ym xwdth ym],'Sum Flex'); ...
    putTxt([xm+8*xwdth*xs 1-ym xwdth ym],'Ack Flex'); ...
    putTxt([xm+9*xwdth*xs 1-ym xwdth*xs ym],'Had lunch');];

set(texth,'Backgroundcolor', [0.4 0.5 0.6], ...
    'HorizontalAlignment','center');

lstflexh = ...
    putFld([xm+8*xwdth*xs 1-ym+yhgt xwdth yhgt], ...
    num2str(lstflex),'micrpt(''update'')', hmnthrpt);
sperx = 1-(ym + (no_days-1)*(yhgt+deltay/2));
urban = axes('position',[0 0 1 1]);
axis([0 1 0 1]); axis off;

for m = 1:no_days
  ypos = 1-(ym + (m-1)*(yhgt+deltay/2));
  turban = line([0 1],[1 1]*(ypos));
  set(turban,'LineStyle',':');

  daynmh(m) = ...
      putTxt([xm ypos xwdth yhgt], day{mod(m+dayshift,7)+1}, hmnthrpt);
      
  daynoh(m) = ...
      putTxt([xm+xs*xwdth ypos xwdth yhgt],num2str(m), hmnthrpt);

  dcodeh(m) = ...
      putFld([xm+2*xs*xwdth ypos xwdth yhgt],'', ...
      'micrpt(''update'')', hmnthrpt);

  if holidays(m)
    dstart1(m) = 0;
    dend1(m) = 0;
  end;
  
  dstart1h(m) = ...
      putFld([xm+xs*xwdth*3 ypos xwdth yhgt],...
      num2tme(dstart1(m)),  ...
      'micrpt(''update'')', hmnthrpt);
  
  dend1h(m) = ...
      putFld([xm+xs*xwdth*4 ypos xwdth yhgt], ...
      num2tme(dend1(m)), ...
      'micrpt(''update'')', hmnthrpt);

  dstart2h(m) = ...
      putFld([xm+xs*xwdth*5 ypos xwdth yhgt],...
      num2tme(dstart2(m)),  ...
      'micrpt(''update'')', hmnthrpt);
  
  dend2h(m) = ...
      putFld([xm+xs*xwdth*6 ypos xwdth yhgt], ...
      num2tme(dend2(m)), ...
      'micrpt(''update'')', hmnthrpt);

  dlunchh(m) = ...
      putChk([xm+xs*xwdth*9 ypos xs*xwdth yhgt], ...
	  'Yes', ...
	  'micrpt(''update'')', hmnthrpt);
       
  if ~(holidays(m))
	dayflex(m) =  ...
	    tme2num(get(dend1h(m),'String')) - ...
	    tme2num(get(dstart1h(m),'String')) + ...
	    tme2num(get(dend2h(m),'String')) - ...
	    tme2num(get(dstart2h(m),'String')) - ...
	    - worktime - lunch*get(dlunchh(m),'Value');
  else
    dayflex(m) =  ...
	tme2num(get(dend1h(m),'String')) - ...
	tme2num(get(dstart1h(m),'String')) - ...
	tme2num(get(dend2h(m),'String')) - ...
	tme2num(get(dstart2h(m),'String')) - ...
	- lunch*get(dlunchh(m),'Value');
  end;

  dayflexh(m) = ...
      putTxt([xm+xs*xwdth*7 ypos xwdth yhgt], ...
      num2str(dayflex(m)), hmnthrpt);

  if (m > 1)
    ackflex(m) = ackflex(m-1) + dayflex(m);
  else
    ackflex(m) = dayflex(m) + str2num(get(lstflexh,'String'));
  end;
  
  ackflexh(m) = ...
      putTxt([xm+xs*xwdth*8 ypos xwdth yhgt], ...
      num2str(ackflex(m)), hmnthrpt);
end;

putFrm([0.05 0.05 0.9 0.1],'Controls and statistics',hmnthrpt);
putBtn([0.1 ym 0.15 yhgt],'Save data', 'micrpt(''save'')');
putBtn([0.1 ym+yhgt*1.1 0.15 yhgt],'Load data', 'micrpt(''load'')');

putTxt([0.1+0.2 ym+yhgt*1.1 0.2 yhgt],'Total worktime:', hmnthrpt);
totwrkh = putTxt([0.1+0.2+0.2 ym+yhgt*1.1 0.1 yhgt], ...
    'djdjd', hmnthrpt);

putTxt([0.1+0.2 ym 0.2 yhgt],'Total flextime:', hmnthrpt);
totackh = putTxt([0.1+0.2+0.2 ym 0.1 yhgt], ...
    'djdjd', hmnthrpt);

set(daynoh(find(holidays)),'Backgroundcolor', [1.0 0.2 0.2]);
set(dcodeh(find(holidays)),'Backgroundcolor', [1.0 0.6 0.6]);
set(daynmh,'Backgroundcolor',[0.2 0.9 0.2]);
set(dlunchh(find(holidays)),'Backgroundcolor', [1.0 0.2 0.2]);
set(dlunchh(find(holidays)),'Value',0,'String','No');
set(daynmh(find(holidays)),'Backgroundcolor', [1.0 0.2 0.2]);
set(dstart2h(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(dend2h(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(dstart1h(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(dend1h(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(dayflexh(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);
set(ackflexh(find(holidays)),'Backgroundcolor', [1.0 0.5 0.5]);

micrpt('update');

elseif (strcmp('save',state))

  fid = fopen([homepath, ...
	  '/month.report.',month,'.txt'],'w');
  for m = 1:no_days
    fwrite(fid, [num2str(m),' ']);
    
    if strcmp('', get(dcodeh(m),'String'))
      fwrite(fid,['nocode ']);
    else
      fwrite(fid, [get(dcodeh(m),'String'),' ']);
    end;
    
    if strcmp('', get(dstart1h(m),'String'))
      fwrite(fid,['0 ']);
    else
      fwrite(fid, [get(dstart1h(m),'String'),' ']);
    end;
    
    if strcmp('', get(dend1h(m),'String'))
      fwrite(fid,['0 ']);
    else
      fwrite(fid, [get(dend1h(m),'String'),' ']);
    end;

    if strcmp('', get(dstart2h(m),'String'))
      fwrite(fid,['0 ']);
    else
      fwrite(fid, [get(dstart2h(m),'String'),' ']);
    end;

    if strcmp('', get(dend2h(m),'String'))
      fwrite(fid,['0 ']);
    else
      fwrite(fid, [get(dend2h(m),'String'),' ']);
    end;

    if get(dlunchh(m),'Value')
      fwrite(fid,['Yes ']);
    else
      fwrite(fid,['No ']);
    end;
    
    fwrite(fid,' break ');
  end;
  fwrite(fid,'data ');
  fwrite(fid,[get(lstflexh,'String'),' ']);
  fwrite(fid,'start1 ');
  fwrite(fid,'end1 ');
  fwrite(fid,'start2 ');
  fwrite(fid,'end2 ');
  fwrite(fid,'lunch ');
  fwrite(fid,'break ');
  fclose(fid);


elseif (strcmp('load',state))
  [dayno, codes, start1, end1, start2, end2, lunch, breakers] = ...
      textread([homepath, ...
	  '/month.report.',month,'.txt'], ...
       '%s %s %s %s %s %s %s %s');
   
   if ~(length(dayno)==(no_days+1));
     warning('Incompatible months!');
   end;
   for m = 1:no_days
     if strcmp(codes{m},'nocode')
       set(  dcodeh(m), 'String',   '');
     else
       set(  dcodeh(m), 'String',   codes{m});
     end;
     set(dstart1h(m), 'String', start1{m});
     set(  dend1h(m), 'String',   end1{m});
     set(dstart2h(m), 'String', start2{m});
     set(  dend2h(m), 'String',   end2{m});
     set( dlunchh(m), 'String', lunch{m});
     if strcmp(get(dlunchh(m),'String'),'Yes')
       set(dlunchh(m),'Value',1);
     else
       set(dlunchh(m),'Value',0);
     end;
   end;
   set(lstflexh,'String',codes{no_days+1});
   micrpt('update');
   
elseif (strcmp('update',state))
  totwrk = 0;
  for m = 1:no_days
    if get(dlunchh(m),'Value')
      set(dlunchh(m),'String','Yes')
    else
      set(dlunchh(m),'String','No')
    end;
    
    if ~(holidays(m))
      dayflex(m) = ...
	  tme2num(get(dend1h(m),'String')) - ...
	  tme2num(get(dstart1h(m),'String')) + ...
	  tme2num(get(dend2h(m),'String')) - ...
	  tme2num(get(dstart2h(m),'String')) - ...
	  worktime - lunch * get(dlunchh(m),'Value');
      totwrk = totwrk + dayflex(m) + worktime + ...
	  lunch * get(dlunchh(m),'Value');
    else
      dayflex(m) = ...
	  tme2num(get(dend1h(m),'String')) - ...
	  tme2num(get(dstart1h(m),'String')) + ...
	  tme2num(get(dend2h(m),'String')) - ...
	  tme2num(get(dstart2h(m),'String')) - ...
	  lunch*get(dlunchh(m),'Value');
      totwrk = totwrk + dayflex(m) + ...
	  lunch * get(dlunchh(m),'Value');
    end;
    set(dayflexh(m),'String', num2str(0.01*round(100*dayflex(m))));
    if (m > 1)
      ackflex(m) = ackflex(m-1) + dayflex(m);
    else
      ackflex(m) = dayflex(m) + str2num(get(lstflexh,'String'));
    end;
    set(ackflexh(m),'String', num2str(0.01*round(100*ackflex(m))));
  end;
  set(totackh,'String', num2str(ackflex(no_days)));
  set(totwrkh,'String', num2str(totwrk));
  get(ackflexh(no_days),'String');
end;
    

