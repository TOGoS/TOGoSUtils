@echo off

if "%1"=="4909" goto init_4909net
if "%1"=="internet" goto init_internet
echo Please specify '4909' or 'internet'>&2
exit /B 1

:init_4909net

setx FS_MARVIN_SSH_PORT 22
setx FS_MARVIN_SSH_HOSTNAME 192.168.9.8
setx TOGOS_FBS_SSH_PORT 22
setx TOGOS_FBS_SSH_HOSTNAME 192.168.9.15

goto eof


:init_internet

setx FS_MARVIN_SSH_PORT 22
setx FS_MARVIN_SSH_HOSTNAME fs.marvin.nuke24.net
setx TOGOS_FBS_SSH_PORT 31522
setx TOGOS_FBS_SSH_HOSTNAME external.marvin.nuke24.net

goto eof


:eof
