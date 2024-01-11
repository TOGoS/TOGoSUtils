@echo off

setlocal
call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-sector" set ccouch_store_sector=%~2
if not defined ccouch_store_sector (echo ccouch_store_sector not specified & goto fail)

set "command=java -jar %CCOUCH3_JAR% upload -repo:%CCOUCH_REPO_NAME% %CCOUCH_REPO_DIR%"

set any_remotes=0

if defined FS_MARVIN_SSH_PORT set "portarg=-P %FS_MARVIN_SSH_PORT%"
if defined FS_MARVIN_SSH_HOSTNAME (set "command=%command% -command-server:fs.marvin plink -batch -P %FS_MARVIN_SSH_PORT% tog@%FS_MARVIN_SSH_HOSTNAME% ccouch3 command-server -repo ~/.ccouch -sector %ccouch_store_sector% ";"" && set any_remotes=1)
if defined TOGOS_FBS_SSH_PORT set "portarg=-P %TOGOS_FBS_SSH_PORT%"
if defined TOGOS_FBS_SSH_HOSTNAME (set "command=%command% -command-server:togos-fbs plink -batch %portarg% tog@%TOGOS_FBS_SSH_HOSTNAME% ccouch3 command-server -repo ~/.ccouch -sector %ccouch_store_sector% ";"" && set any_remotes=1)

if %any_remotes% EQU 0 (echo No remotes specified; plz set FS_MARVIN_SSH_HOSTNAME [and _PORT] and/or TOGOS_FBS_SSH_HOSTNAME [and _PORT])

set command=%command% %*

@echo on
%command%
@echo off
if errorlevel 1 goto fail

goto eof

:fail
echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
