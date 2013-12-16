
disp('%');
disp(['% User: ', daisyUnixCmd('echo $USER', 'unknown') ]);
disp(['% Project name: ', daisyUnixCmd('echo $PROJNAME', 'unknown') ]);
disp(['% Project area: ', daisyUnixCmd('echo $PROJAREA', 'unknown') ]);
disp('%');
disp('% Department of Electrical Engineering');
disp('% Linkoping University ');
disp('%');
[res, pat] = unix('date');
disp(['% ', pat(1:(end-1))]);
disp('%');
