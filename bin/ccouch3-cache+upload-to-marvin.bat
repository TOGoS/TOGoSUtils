@echo off

setlocal
set cc3cau_any_cache_errors=
set self_name=%~n0
set exitcode=0

call ccouch3-cache.bat %*
if errorlevel 1 set cc3cau_any_cache_errors=1
call ccouch3-upload-to-marvin.bat %*
if errorlevel 1 set cc3cau_any_upload_errors=1

if defined cc3cau_any_cache_errors=1 echo %self_name%: There were caching failures>&2
if defined cc3cau_any_upload_errors=1 echo %self_name%: There were upload failures>&2

if defined cc3cau_any_cache_errors exit /B 1
if defined cc3cau_any_upload_errors exit /B 1
exit /B 0
