@echo off

setlocal

if not defined MAVEN_REPOSITORY_DIR set "MAVEN_REPOSITORY_DIR=%UserProfile%\.m2\repository"

set "clojure_cp=%MAVEN_REPOSITORY_DIR%\org\clojure\clojure\1.12.3\clojure-1.12.3.jar;%MAVEN_REPOSITORY_DIR%\org\clojure\spec.alpha\0.5.238\spec.alpha-0.5.238.jar;%MAVEN_REPOSITORY_DIR%\org\clojure\core.specs.alpha\0.4.74\core.specs.alpha-0.4.74.jar"

java -cp "%clojure_cp%" clojure.main %*
