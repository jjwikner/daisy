function smiundo
%SMIUNDO Undoes the last operation in the Smith Chart.

global SMITH_CHART_UNDO
if SMITH_CHART_UNDO ~= []
  delete(SMITH_CHART_UNDO(length(SMITH_CHART_UNDO)));
  SMITH_CHART_UNDO = SMITH_CHART_UNDO(1:length(SMITH_CHART_UNDO)-1);
else
  error('No more undoes possible');
end