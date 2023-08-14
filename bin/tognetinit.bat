@echo off

set "tognetinit_network=%~1"

if "%tognetinit_network%"=="4909" goto init_4909net
if "%tognetinit_network%"=="1301" goto init_internet
if "%tognetinit_network%"=="internet" goto init_internet
echo Please specify '4909' or 'internet'>&2
exit /B 1

:init_4909net

set "FS_MARVIN_SSH_PORT=22"
set "FS_MARVIN_SSH_HOSTNAME=192.168.9.8"
set "TOGOS_FBS_SSH_PORT=22"
set "TOGOS_FBS_SSH_HOSTNAME=192.168.9.15"
set "CCOUCH3_TOGNET_REMOTE_ARGS=-remote-repo:togos-fbs 192.168.9.15 -remote-repo:fs.marvin 192.168.9.8"

goto setx


:init_internet

set "FS_MARVIN_SSH_PORT=22"
set "FS_MARVIN_SSH_HOSTNAME=fs.marvin.nuke24.net"
set "TOGOS_FBS_SSH_PORT=31522"
set "TOGOS_FBS_SSH_HOSTNAME=external.marvin.nuke24.net"
set "CCOUCH3_TOGNET_REMOTE_ARGS=-remote-repo:togos-fbs external.marvin.nuke24.net:31580 -remote-repo:fs.marvin fs.marvin.nuke24.net"

goto setx


:setx
setx FS_MARVIN_SSH_PORT %FS_MARVIN_SSH_PORT%
setx FS_MARVIN_SSH_HOSTNAME %FS_MARVIN_SSH_HOSTNAME%
setx TOGOS_FBS_SSH_PORT %TOGOS_FBS_SSH_PORT%
setx TOGOS_FBS_SSH_HOSTNAME %TOGOS_FBS_SSH_HOSTNAME%
setx CCOUCH3_TOGNET_REMOTE_ARGS "%CCOUCH3_TOGNET_REMOTE_ARGS%"

if not exist C:\Windows\system32\drivers\etc\always.hosts goto eof

type C:\Windows\system32\drivers\etc\always.hosts > C:\Windows\system32\drivers\etc\hosts
if exist "C:\Windows\system32\drivers\etc\%tognetinit_network%.hosts" type "C:\Windows\system32\drivers\etc\%tognetinit_network%.hosts" >> C:\Windows\system32\drivers\etc\hosts

:eof

set "tognetinit_network="
