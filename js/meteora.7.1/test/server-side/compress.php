<?php

/**
* GZIP Compression for Meteora
* -----------------------
* Written by Jose Carlos Nieto <xiam@astrata.com.mx>
* (c) 2008-2009 Astrata Software http://astrata.com.mx
*
*/

define('METEORA_LIBPATH', './meteora/lib/');
define('CACHE_DIR', '../temp/');
define('ENABLE_GZIP', true);

$src =& $_GET['src'];

error_reporting(0);

if (!empty($src)) {
        
	$src = preg_replace('/[^a-zA-Z0-9\.,]/i', '', $src);

	$etag = md5($src);
	
	if ($_SERVER['HTTP_IF_NONE_MATCH'] == $etag) {
		header('HTTP/1.0 304 Not Modified');
		exit;
	}

  $files = explode(',', $src);

  if ($files) {
  
    header('Content-Type: text/javascript; charset=utf-8');
		header('Cache-Control: public');
		header('Expires: '.gmdate('D, d M Y H:i:s', time()+3600).' GMT');
		header('Etag: '.$etag.'');

    $cache_file = CACHE_DIR.'js_'.$etag;

    if (file_exists($cache_file)) {
      if (ENABLE_GZIP) {
        header('Content-Encoding: gzip');
        header('Vary: Accept-Encoding');
      }
      readfile($cache_file);
    } else {
      if (ENABLE_GZIP) { 
        ob_start('ob_gzhandler');
      } else {
        ob_start();
      }
      
      foreach($files as $file) {
        $file = METEORA_LIBPATH.str_replace('.', '/', $file).'.js';
        if (file_exists($file)) {
          readfile($file);
        }
      }

      $buff = ob_get_clean();
      if (ENABLE_GZIP) {
        $size = strlen($buff);

        header('Content-Encoding: gzip');
        header('Vary: Accept-Encoding');

        $crc = crc32($buff);
        $buff = "\x1f\x8b\x08\x00\x00\x00\x00\x00".substr(gzcompress($buff, 9), 0, -4);
        $buff .= pack('V', $crc);
        $buff .= pack('V', $size);
      }

      $fh = fopen($cache_file, 'w');
      fwrite($fh, $buff);
      fclose($fh);

      echo $buff;
    }
  }
}

?>
