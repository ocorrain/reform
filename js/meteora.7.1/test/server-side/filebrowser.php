<?php

/**
 * filebrowser demo
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

function file_info($fullpath) {
  $base = basename($fullpath);
  // basic information
  $info = array(
    // full webroot relative path
    'path'    => str_replace(ROOT, '', $fullpath),
    // extension
    'ext'     => preg_replace('/^.*\.([^\.]+)$/', '\1', strtolower($base)),
    // name
    'name'    => $base,
    // size (bytes)
    'size'    => @filesize($fullpath),
    // type
    'type'    => array(
      // true if the file is a directory
      'dir'     => is_dir($fullpath),
      // true if the file is a unix symlink
      'link'    => is_link($fullpath),
      // true if the file is a normal file
      'file'    => is_file($fullpath)
    )
  );
  // this is the code that will be displayed
  $info['html'] = '<b>'.$base.'</b> <i>'.$info['size'].' bytes</i>';
  if ($info['type']['dir']) {
    // using a blue color to make a difference between files and directories
    $info['html'] = '<span style="color: #06c">'.$info['html'].'</span>';
  } 
  return $info;
}

// system's root
define('ROOT', dirname(dirname(dirname(__FILE__))).'/');

$path = param('path');

// security check
if (preg_match('/\.\./', $path) || !$path) {
  $path = '/';
}

$fullpath = ROOT.$path;

// checking if the file exists
if (file_exists($fullpath)) {
  $files = array();

  // getting information of every file if $fullpath is a directory
  if (is_dir($fullpath)) {
    $dh = opendir($fullpath);
    while (($file = readdir($dh)) !== false) {
      if ($file != '.' && $file != '..') {
        // file information for the current file
        $files[] = file_info($fullpath.'/'.$file);
      }
    }
    closedir($dh);
  }

  // the final result
  $result = array(
    // the current path (webroot relative)
    'path' => $path,
    // file information for this directory
    'info' => file_info($fullpath),
    // an array of file information (is $path is a directory)
    'files' => $files
  );

  // json formatted output
  $json = new json();
  print $json->encode($result);

}

?>
