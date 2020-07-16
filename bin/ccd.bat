@echo off

rem Change directory, but:
rem - Also switch drive
rem - Also set the window title to the last path section

%~d1
title %~n1
cd %1
