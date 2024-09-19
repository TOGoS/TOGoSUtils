@call require-ccouch-env.bat

ccouch3 web-server -port 80 -repo:%CCOUCH_REPO_NAME% %CCOUCH_REPO_DIR% -union:/ccouch %CCOUCH_REPO_DIR%
