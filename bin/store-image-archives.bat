@echo off

setlocal

set script_version=2022-04-12
set script_name_and_version=%~nx0 v%script_version%

call require-ccouch-env.bat
if errorlevel 1 goto fail

if "%~1" EQU "-m" set commit_message=%~2
if not defined commit_message set commit_message=Image archives on %CCOUCH_REPO_NAME%
rem TODO: datastore_root isn't really required if image_archives_dir is set...
if not defined datastore_root (echo datastore_root not specified >&2 & goto fail)
if not defined image_archives_dir set image_archives_dir=%datastore_root%\archives\images
if not defined ccouch_store_sector set ccouch_store_sector=pictures

@echo on
java -jar %CCOUCH_JAR% -repo:%CCOUCH_REPO_NAME% %CCOUCH_REPO_DIR% store ^
	-link ^
	-sector %ccouch_store_sector% ^
	-n archives/images ^
	-a "%script_name_and_version%" ^
	-use-uri-dot-files ^
	-create-uri-dot-files ^
	-m "%commit_message%" ^
	%image_archives_dir%
@if errorlevel 1 goto fail
@echo off

: echo:
: echo Head stored...
: call togutil list-ccouch-heads eng-lap-426-2019/archives/images --last=1
: echo You must upload that manually for now.

call ccouch3-upload-to-marvin -recurse x-ccouch-head:%CCOUCH_REPO_NAME%/archives/images/latest

mkdir %UserProfile%\temp
set push_image_archives_bat=%UserProfile%\temp\push-image-archives.bat
del %push_image_archives_bat%
call togutil list-ccouch-heads %CCOUCH_REPO_NAME%/archives/images ^
	--recurse --last=1 ^
	--output-format="" ^
	--output-format+="plink -P %FS_MARVIN_SSH_PORT% -batch tog@%FS_MARVIN_SSH_HOSTNAME% mkdir -p ""/home/tog/.ccouch/heads/{parentName}""{nl}" ^
	--output-format+="pscp -P %FS_MARVIN_SSH_PORT% {file} ""tog@%FS_MARVIN_SSH_HOSTNAME%:/home/tog/.ccouch/heads/{parentName}/""{nl}" ^
	--output-format+="plink -P %TOGOS_FBS_SSH_PORT% -batch tog@%TOGOS_FBS_SSH_HOSTNAME% mkdir -p ""/home/tog/.ccouch/heads/{parentName}""{nl}" ^
	--output-format+="pscp -P %TOGOS_FBS_SSH_PORT% {file} ""tog@%TOGOS_FBS_SSH_HOSTNAME%:/home/tog/.ccouch/heads/{parentName}/""{nl}" ^
	> %push_image_archives_bat%
echo Generated push script, %push_image_archives_bat%
@echo on
@call %push_image_archives_bat%
@echo off

:stdbatfooter
goto eof
:fail
@echo %~f0 exiting with errorlevel 1 due to errors >&2
exit /B 1
:eof
