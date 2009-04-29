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

mysql_connect('localhost', 'wikipedia', 'wikipedia');
mysql_select_db('wikipedia');

$results = array();

$limit = 8;

function stripchars($el) {
  $el = preg_replace('/[^a-zA-Z0-9áéíóúÁÉÍÓÚ]/', '', $el);
  return $el;
}

$partial  = stripchars(param('partial'));
$linked   = stripchars(param('linked'));

if ($partial) {
  if ($linked) {
    $q = mysql_query("SELECT id FROM words WHERE word = '{$linked}'");
    $r = mysql_fetch_row($q);
    if ($r) {
      $q = mysql_query("
        SELECT w.word FROM words w INNER JOIN wordlinks wl ON
        wl.link_to = w.id AND wl.link_from = '{$r[0]}'
        WHERE w.word LIKE '{$partial}%'
        ORDER BY wl.rank DESC LIMIT {$limit}
      ");
      while ($r = mysql_fetch_row($q)) {
        $results[] = $r[0];
      }
    }
  } else {
    $q = mysql_query("SELECT word FROM words WHERE word LIKE '{$partial}%' ORDER BY occurrences DESC LIMIT {$limit}");
    while($r = mysql_fetch_row($q)) {
      $results[] = $r[0];
    }
  }
} else if ($linked) {
  $q = mysql_query("SELECT id FROM words WHERE word = '{$linked}'");
  $r = mysql_fetch_row($q);
  if ($r) {
    $q = mysql_query("
      SELECT w.word FROM words w INNER JOIN wordlinks wl ON
      wl.link_to = w.id AND wl.link_from = '{$r[0]}'
      ORDER BY wl.rank DESC LIMIT {$limit}
    ");
    while ($r = mysql_fetch_row($q)) {
      $results[] = $r[0];
    }
  }
}

foreach($results as $i => $v) {
  $results[$i] = strtolower($v);
}

natsort($results);

$json = new json();
if ($results) {
  // success
  echo $json->encode($results);
} else {
  // error message
  echo $json->encode(null);
}

?>
