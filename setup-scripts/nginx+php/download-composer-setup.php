<?php

copy("https://getcomposer.org/installer", "composer-setup.temp.php");
$expectedHash = trim(file_get_contents("https://composer.github.io/installer.sig"));
if( hash_file("SHA384", "composer-setup.temp.php") !== $expectedHash ) {
    throw new Exception("composer-setup.php hash mismatch!");
}
rename("composer-setup.temp.php", "composer-setup.php");
