<?php
// returns a variable value, if setted, or null
function param($name) {
  return isset($_POST[$name]) ? $_POST[$name] : (isset($_GET[$name]) ? $_GET[$name] : null);
}
?>
