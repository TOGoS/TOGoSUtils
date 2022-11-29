@echo off
@setlocal

if not defined PA4_JAR goto find_pa4_jar
goto run_pa4

:find_pa4_jar

set "PA4_JAR=%UserProfile%\stuff\proj\PictureArchiver4.1\PA4.jar"
if exist "%PA4_JAR%" goto configure

echo Error: Couldn't find PA4.jar in usual location(s), and PA4_JAR not set.>&2
exit /B 1

:configure
if defined TOG_DATASTORE_DIR goto run_pa4

echo Error: TOG_DATASTORE_DIR not set.  Maybe it should be "D:" or somesuch.>&2
exit /B 1



:run_pa4
java -jar "C:\Users\togos\stuff\proj\PictureArchiver4.1\PA4.jar" ^
	-archive-map "%TOG_DATASTORE_DIR%/incoming" "%TOG_DATASTORE_DIR%/archives" ^
	%*
exit /B %ERRORLEVEL%
