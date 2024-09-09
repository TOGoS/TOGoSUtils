if defined MVN_CMD goto mvn_cmd_defined
if exist C:\Users\stevedxfph\apps\apache-maven-3.8.6\bin\mvn.cmd set "MVN_CMD=C:\Users\stevedxfph\apps\apache-maven-3.8.6\bin\mvn.cmd" && goto mvn_cmd_defined
if exist C:\Users\stevedxfph\apps\apache-maven-3.9.8\bin\mvn.cmd set "MVN_CMD=C:\Users\stevedxfph\apps\apache-maven-3.9.8\bin\mvn.cmd" && goto mvn_cmd_defined

echo MVN_CMD not defined and couldn't find it.>&2
exit /B 1

:mvn_cmd_defined
