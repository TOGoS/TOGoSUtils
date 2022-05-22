@echo off

setlocal
call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-sector" set ccouch_store_sector=%~2
if not defined ccouch_store_sector (echo ccouch_store_sector not specified & goto fail)

set command=java -jar %ccouch3_jar% upload -repo:%ccouch_repo_name% %ccouch_repo_dir%

set any_remotes=0

if defined fs_marvin_ssh_port (set command=%command% -command-server:fs.marvin plink -batch -P %fs_marvin_ssh_port% tog@%fs_marvin_ssh_hostname% ccouch3 command-server -repo ~/.ccouch -sector %ccouch_store_sector% ";" && set any_remotes=1)
if defined togos_fbs_ssh_port (set command=%command% -command-server:togos-fbs plink -batch -P %togos_fbs_ssh_port% tog@%togos_fbs_ssh_hostname% ccouch3 command-server -repo ~/.ccouch -sector %ccouch_store_sector% ";" && set any_remotes=1)

if %any_remotes% EQU 0 (echo No remotes specified; plz set fs_marvin_ssh_port and/or togos_fbs_ssh_port)

set command=%command% %*

@echo on
%command%
if errorlevel 1 goto fail
	
@echo off
goto eof

:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
