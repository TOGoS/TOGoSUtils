@echo off

setlocal

set "self_name=%~nx0"
set "self_dir=%~dp0"
set "pause_if_errors="
rem `shift` doesn't affect %*.  So do it like this:
set "remaining_args="
set "own_window="


:process_arg
if "" == "%1" goto no_more_args
if "--pause-if-errors" == "%~1" (set "pause_if_errors=1" & goto process_next_arg)
if "--own-window" == "%~1" (set "own_window=1" & set "pause_if_errors=1" & goto process_next_arg)
rem Otherwise add it to the list
rem echo # set "remaining_args=%remaining_args% %1"
set "remaining_args=%remaining_args% %1"
rem echo # Remaining args: %remaining_args%
:process_next_arg
shift
goto process_arg


:no_more_args

if defined own_window title Morning pull!

rem echo Remaining args: %remaining_args%
rem echo # Running: node %self_dir%morning-pull%remaining_args%
node %self_dir%morning-pull%remaining_args%
if errorlevel 1 goto fail
if defined own_window exit 0
goto eof


:fail
echo %self_name%: Oh no, there were some errors.
if not defined pause_if_errors goto fail_exit
echo %self_name%: Pausing because --pause-if-error
pause
goto fail_exit


:fail_exit
if defined own_window exit 1
exit /B 1



:eof
