function result = cinputdialogs(DlgName, DlgTxt, DlgDef, DlgMode)
%res = cinputdialogs('hej,'du','glade', 0);
global contloop result deleted figinp OKCallback CancelCallback;
  if DlgMode == 0
	contloop = 1;
	OKCallback = 'cinputdialogs(''DlgName'',''DlgTxt'', ''DlgDef'',2)';
	CancelCallback = 'cinputdialogs(''DlgName'',''DlgTxt'', ''DlgDef'',4)';
	figinp = inputbox(DlgTxt,DlgName, [OKCallback], [CancelCallback], DlgDef);
	result = [];
	deleted = 0;
        while contloop
	  pause(1);
  	end;
	if ~deleted 
	  cinputdialogs(DlgName, DlgTxt, DlgDef, 4);
	else
	  close;
	end;
  elseif DlgMode == 2
	result = get(findobj(figinp,'Style','edit'),'String');
	contloop = 0;
	delete(figinp);
	deleted = 1;
	close;
  elseif DlgMode == 4
	contloop = 0;
	if ~deleted 
		delete(figinp);
	end;
	deleted = 1;
	result = [];
	close;  
  end;





