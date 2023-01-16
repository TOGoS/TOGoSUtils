@echo off

call require-ccouch-env.bat
if errorlevel 1 exit /B 1

ls %CCOUCH_REPO_DIR%\data | cat
