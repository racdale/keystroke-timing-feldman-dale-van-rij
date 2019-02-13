<?php

$myFile = "data/".$_SERVER["REMOTE_ADDR"].".txt";
$fh = fopen($myFile, 'a');
fwrite($fh,$_POST["dataBlob"]."\n");
fclose($fh);

?>
