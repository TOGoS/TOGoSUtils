@echo off

set comman=%*

:begin
call %comman%
if errorlevel 1 goto loop
if errorlevel 0 goto eof
:loop
sleep 5
goto begin
:eof
