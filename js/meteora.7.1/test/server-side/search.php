<?php

/**
 * search demo
 * ---
 * Written by José Carlos Nieto <xiam@astrata.com.mx>
 * Copyright (c) 2007 Astrata Software S.A. de C.V.
 *
 * Licensed under The MIT License
 * Redistributions of files must retain the above copyright notice.
 *
 * @author          José Carlos Nieto <xiam@astrata.com.mx>
 * @copyright       Copyright (c) 2007, Astrata Software S.A. de C.V.
 * @license         http://www.opensource.org/licenses/mit-license.php The MIT License
 */

include 'common.php';
include 'json.php';

define('WORDS', 'words.txt');

// the final value
$results = array();

// removing non letters nor numbers
$q = preg_replace('/[^a-zA-Z0-9\s\.]/', '', param('q'));

if ($q) {

  // opening this file for reading
  $fp = fopen(WORDS, 'r');
  $buff = fread($fp, filesize(WORDS));
  fclose($fp);

  $buff = explode("\n", $buff);

  // case does matter
  $mods = param('case_matters') ? '' : 'i';
  
  // adding matching lines to $results
  foreach($buff as $line) {
    if (preg_match('/('.preg_quote($q).')/'.$mods, $line)) {
      $code = substr($line, 0, 2);
      $name = substr($line, 4);
      // option: display
      switch(param('display')) {
        case 2:
          $results[$code] = trim($code);
        break;
        case 3:
          $results[$code] = trim($code).' '.trim($name);
        break;
        default:
          $results[$code] = trim($name);
        break;
      }
    }
  }

  // option: order
  if (param('order') == 'desc') {
    arsort($results);
  } else {
    asort($results);
  }

}

$json = new json();
if ($results) {
  // success
  print $json->encode($results);
} else {
  // error message
  print $json->encode(
    array(
      'jsonRpc' => array(
        'errorMessage' => 'No results.'
      )
    )
  );
}

?>
