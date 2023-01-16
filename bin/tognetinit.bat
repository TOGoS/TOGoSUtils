@echo off

if "%~1"=="4909" goto init_4909net
if "%~1"=="internet" goto init_internet
echo Please specify '4909' or 'internet'>&2
exit /B 1

:init_4909net

set "FS_MARVIN_SSH_PORT=22"
set "FS_MARVIN_SSH_HOSTNAME=192.168.9.8"
set "TOGOS_FBS_SSH_PORT=22"
set "TOGOS_FBS_SSH_HOSTNAME=192.168.9.15"

goto setx


:init_internet

set "FS_MARVIN_SSH_PORT=22"
set "FS_MARVIN_SSH_HOSTNAME=fs.marvin.nuke24.net"
set "TOGOS_FBS_SSH_PORT=31522"
set "TOGOS_FBS_SSH_HOSTNAME=external.marvin.nuke24.net"

goto setx


:setx
setx FS_MARVIN_SSH_PORT %FS_MARVIN_SSH_PORT%
setx FS_MARVIN_SSH_HOSTNAME %FS_MARVIN_SSH_HOSTNAME%
setx TOGOS_FBS_SSH_PORT %TOGOS_FBS_SSH_PORT%
setx TOGOS_FBS_SSH_HOSTNAME %TOGOS_FBS_SSH_HOSTNAME%

:eof
