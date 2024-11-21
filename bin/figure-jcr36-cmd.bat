if defined JCR36_CMD goto jcr36_cmd_defined
if not defined JAVA_EXE set JAVA_EXE=java
if defined JCR36_CP goto define_jcr36_cmd_using_cp
if defined JCR36_JAR goto define_jcr36_cmd_using_jar

set "JCR36_JAR=%UserProfile%\stuff\proj\JavaCommandRunner36\JCR36.1.30.jar"
if exist %JCR36_JAR% goto define_jcr36_cmd_using_jar

set "JCR36_JAR=%UserProfile%\stuff\proj\JavaCommandRunner36\JCR36.1.28.jar"
if exist %JCR36_JAR% goto define_jcr36_cmd_using_jar

goto fail_no_jar


:define_jcr36_cmd_using_cp
set "JCR36_CMD="%JAVA_EXE%" -cp "%JCR36_CP%" net.nuke24.jcr36.SimplerCommandRunner"
goto jcr36_cmd_defined

:define_jcr36_cmd_using_jar
set "JCR36_CMD="%JAVA_EXE%" -jar "%JCR36_JAR%""
goto jcr36_cmd_defined

:fail_no_jar
echo JC336_JAR not defined and couldn't find it>&2
echo (guess '%JCR36_JAR% doesn't exist).>&2
exit /B 1

:jcr36_cmd_defined
