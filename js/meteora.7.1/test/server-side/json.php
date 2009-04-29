<?php

/**
 * JSON Class
 * ---
 * Written by J. Carlos Nieto <xiam@menteslibres.org>
 * Copyright (c) 2007-2009 J. Carlos Nieto
 *
 * Licensed under The MIT License
 * Redistributions of files must retain the above copyright notice.
 *
 * @author          J. Carlos Nieto <xiam@menteslibres.org>
 * @author          Jorge Medrano <me@h1pp1e.net>
 * @author          Michal Migurski <mike-json@teczno.com>
 * @package         http://textmotion.org
 * @copyright       Copyright (c) 2007-2009, J. Carlos Nieto
 * @license         http://www.opensource.org/licenses/mit-license.php The MIT License
 */

define('JSON_PARSE',          0);
define('JSON_PARSE_UNQUOTE',  1);
define('JSON_PARSE_STRING',   2);
define('JSON_PARSE_DELIM',    3);
define('JSON_PARSE_VALUE',    4);
define('JSON_PARSE_OBJECT',   5);
define('JSON_PARSE_ARRAY',    6);
define('JSON_PARSE_NESTED',   7);

class json {

  public function response($mixed) {

    header('Content-Type: text/plain; charset=utf-8');

    if (strpos('msie', $_SERVER['HTTP_USER_AGENT'])) {
      // Avoid IE bug when the output containts HTML tags
      echo str_repeat("\n", 130);  
    }
    
    echo $this->encode($mixed);

    exit;
  }

  public function encode_string($string, $format = false) {

    $string = "$string";

    if ($format) {
      return "\"".str_replace('"', '\"', $string)."\"";
    }

    $count = strlen($string);

    $buff = null;
    
    for ($i = 0; $i < $count; $i++) {
      $curr = $string{$i};
      $ascv = ord($curr);
      switch(true) {
        case $curr == "\t": $curr = '\t'; break;
        case $curr == "\b": $curr = '\b'; break;
        case $curr == "\n": $curr = '\n'; break;
        case $curr == "\f": $curr = '\f'; break;
        case $curr == "\r": $curr = '\r'; break;
        case $curr == '"' || $curr == '\\' || $curr == '/':
          $curr = '\\'.$curr;
        break;
        case $ascv >= 0x20 && $ascv <= 0x7f:
          
        break;
        case ($ascv & 0xe0) == 0xc0:
          $char = pack('C*', $ascv, ord($string{++$i}));
          $utf16 = $this->utf8_to_utf16($char);
          $curr = sprintf('\u%04s', bin2hex($utf16));
        break;
        case ($ascv & 0xf0) == 0xe0:
          $char = pack(
            'C*',
            $ascv,
            ord($string{++$i}),
            ord($string{++$i})
          );
          $utf16 = $this->utf8_to_utf16($char);
          $curr = sprintf('\u%04s', bin2hex($utf16));
        break;
        case ($ascv & 0xf8) == 0xf0:
          $char = pack(
            'C*',
            $ascv,
            ord($string{++$i}),
            ord($string{++$i}),
            ord($string{++$i})
          );
          $utf16 = $this->utf8_to_utf16($char);
          $curr = sprintf('\u%04s', bin2hex($utf16));
        break;
        case ($ascv & 0xfc) == 0xf8:
          $char = pack(
            'C*',
            $ascv,
            ord($string{++$i}),
            ord($string{++$i}),
            ord($string{++$i}),
            ord($string{++$i})
          );
          $utf16 = $this->utf8_to_utf16($char);
          $curr = sprintf('\u%04s', bin2hex($utf16));
        break;
        case ($ascv & 0xfe) == 0xfc:
          $char = pack(
            'C*',
            $ascv,
            ord($string{++$i}),
            ord($string{++$i}),
            ord($string{++$i}),
            ord($string{++$i}),
            ord($string{++$i})
          );
          $utf16 = $this->utf8_to_utf16($char);
          $curr = sprintf('\u%04s', bin2hex($utf16));
        break;
      }
      $buff .= $curr;
    }

    return '"'.$buff.'"';
  }

  // Originally written by Jorge Medrano
  // License MIT
  public function encode($mixed, $format = false) {
    switch (gettype($mixed)) {
      case 'boolean':
        return $mixed ? 'true' : 'false';
      break;
      case 'double': case 'float': case 'integer':
        return $mixed;
      break;
      case 'string':
        return $this->encode_string($mixed, $format);
      break;
      case 'array':
        $buff = array();
        if (!count($mixed) || array_keys($mixed) === range(0, count($mixed)-1)) {
          foreach($mixed as $val) {
            $buff[] = $this->encode($val, $format);
          }
          if ($format) {
            return "[\n".implode($buff, ",\n")."\n]\n";
          } else {
            return '['.implode($buff, ',').']';
          }
        } else {
          foreach($mixed as $key => $val) {
            $buff[] = $this->encode_string($key, $format).':'.$this->encode($val, $format);
          }
          if ($format) {
            return "{\n".implode($buff, ",\n")."\n}\n";
          } else {
            return '{'.implode($buff, ',').'}';
          
          }
          
        }
      break;
      case 'object':
        return $this->encode(get_object_vars($mixed), $format);
      break;
      default:
        return 'null';
      break;
    }
  }

  // Taken from: http://mike.teczno.com/JSON/JSON.phps
  // MIT License.
  public function utf8_to_utf16($utf8) {
    
    if(function_exists('mb_convert_encoding')) {
      return mb_convert_encoding($utf8, 'UTF-16', 'UTF-8');
    }

    switch(strlen($utf8)) {
      case 1:
        return $utf8;
      break;
      case 2:
        // return a UTF-16 character from a 2-byte UTF-8 char
        // see: http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
        return chr(0x07 & (ord($utf8{0}) >> 2))
        . chr((0xC0 & (ord($utf8{0}) << 6))
        | (0x3F & ord($utf8{1})));
      break;
      case 3:
        // return a UTF-16 character from a 3-byte UTF-8 char
        // see: http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
        return chr((0xF0 & (ord($utf8{0}) << 4))
        | (0x0F & (ord($utf8{1}) >> 2)))
        . chr((0xC0 & (ord($utf8{1}) << 6))
        | (0x7F & ord($utf8{2})));
      break;
    }
  }

  // Taken from: http://mike.teczno.com/JSON/JSON.phps
  // MIT License.
  public static function utf16_to_utf8($utf16) {

    if (function_exists('mb_convert_encoding')) {
      return mb_convert_encoding($utf16, 'UTF-8', 'UTF-16');
    }
      
    $bytes = (ord($utf16{0}) << 8) | ord($utf16{1});

    switch(true) {
      case (0x7f & $bytes) == $bytes:
        return chr($bytes);
      break;
      case (0x07ff & $bytes) == $bytes:
        // return a 2-byte UTF-8 character
        // see: http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
        return chr(0xc0 | (($bytes >> 6) & 0x1f))
              .chr(0x80 | ($bytes & 0x3f));
      break;
      case (0xffff & $bytes) == $bytes:
        // return a 3-byte UTF-8 character
        // see: http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
        return chr(0xe0 | (($bytes >> 12) & 0x0f))
              .chr(0x80 | (($bytes >> 6) & 0x3f))
              .chr(0x80 | ($bytes & 0x3f));
      break;
    }

    return null;
  }

  public function decode_value($string) {

    $count = strlen($string);
    $buff = null;
    for ($i = 0; $i < $count; $i++) {
      $curr = $string{$i};
      if ($curr == '\\') {
        if (($i+1) < $count) {

          $next = $string{$i+1};

          switch (true) {
            case '"' == $next && $string{0} == $next: case "'" == $next && $string{0} == $next: case '\\' == $next: case '/' == $next:
              $buff .= $next;  
            break;
            case 'b' == $next:
              $buff .= "\b";
            break;
            case 'f' == $next:
              $buff .= "\f";
            break;
            case 'n' == $next:
              $buff .= "\n";
            break;
            case 'r' == $next:
              $buff .= "\r";
            break;
            case 't' == $next:
              $buff .= "\t";
            break;
            case preg_match('/u[0-9a-f]{4}/i', substr($string, $i+1, 5)):
              $ucode = chr(hexdec(substr($string, $i+2, 2)));
              $ucode .= chr(hexdec(substr($string, $i+4, 2)));
              $buff .= $this->utf16_to_utf8($ucode);
              $i += 4;
            break;
          }
          $i++;
        } else {
          return null;
        }
      } else {
        if ($i > 0 && $i+1 < $count) {
          $buff .= $curr;
        }
      }
    }
    return $buff;
  }

  public function decode($string) {

    $string = trim($string);

    $count = strlen($string);

    $stack = array();

    $json = null;

    $end = $count - 1;

    if ($string == '[]' || $string == '{}') {
      return array();
    } else if ($string{0} == '[' && $string{$end} == ']') {
      $mode = JSON_PARSE_ARRAY;
    } else if ($string{0} == '{' && $string{$end} == '}') {
      $mode = JSON_PARSE_OBJECT;
    } else if ($string == 'true') {
      return true;
    } else if ($string == 'false') {
      return false;
    } else if ($string == 'null') {
      return null;
    } else {
      return null;
    }
    
    $status = JSON_PARSE;
    $json = array();
    $key = null;

    for ($i = 1; $i < $count; $i++) {
    
      $curr = $string{$i};

      if ($status == JSON_PARSE) {
        $buff = null;

        if (preg_match('/[0-9a-z]/i', $curr)) {
          $status = JSON_PARSE_UNQUOTE;
          $i--;
        } else if ($curr == '{' || $curr == '[') {
          $delim = -1;
          $status = JSON_PARSE_NESTED;
          $i--;
        } else if ($curr == '"' || $curr == "'") {
          $delim = -1;
          $status = JSON_PARSE_STRING;
          $i--;
        } else if ($curr == ' ' || $curr == "\t" || $curr == "\n" || $curr == "\r" || $curr == "\f") {
          // white space
        } else {
          trigger_error("Unexpected '{$curr}' at char {$i}", E_USER_ERROR);
        }

      } else if ($status == JSON_PARSE_NESTED) {

        if ($delim < 0) {
          $delim = 1;
          $stack = array();
        }

        if ($curr == '{' || $curr == '[') {
          $stack[] = $i;  
        } else if ($curr == '}' || $curr == ']') {
          if ($stack) {
            $j = array_pop($stack);
            $s = $string{$j};
            
            if (($s == '{' && $curr == '}') || ($s == '[' && $curr == ']')) {
              if (count($stack) == 0) {
                $buff = $this->decode(substr($string, $j, $i - $j + 1));
                $status = JSON_PARSE_DELIM;
              }
            } else {
              trigger_error("Syntax error at $i", E_USER_ERROR);
            }

          } else {
            trigger_error("Syntax error at $i", E_USER_ERROR);
          }
        }

      } else if ($status == JSON_PARSE_UNQUOTE) {
        if (preg_match('/[0-9a-z]/i', $curr)) {
          $buff .= $curr;
        } else {
          $status = JSON_PARSE_DELIM;
          $i--;
        }
      } else if ($status == JSON_PARSE_STRING) {
        $buff .= $curr;
        if ($delim < 0) {
          $delim = $i;
        } else {
          if ($string{$delim} == $curr) {
            if ($string{$i-1} != '\\') {
              $buff = $this->decode_value($buff);
              $status = JSON_PARSE_DELIM;
            }
          }
        }
      } else if ($status == JSON_PARSE_DELIM) {
        if ($curr == ' ' || $curr == "\t" || $curr == "\n" || $curr == "\r" || $curr == "\f") {
          // ignoring white space
        } else {
          if ($mode == JSON_PARSE_ARRAY) {
            if ($curr == ',' || $curr == ']') {
              $json[] = $buff;
              $status = JSON_PARSE;
            } else if ($curr == ' ' || $curr == "\t" || $curr == "\n" || $curr == "\r" || $curr == "\f") {
              // ignoring white space
            } else {
              trigger_error("Got {$curr}, expecting delimiter.", E_USER_ERROR);
            }
          } else if ($mode == JSON_PARSE_OBJECT) {
            if ($curr == ':') {
              if ($key) {
                trigger_error('Expecting value.', E_USER_ERROR);
              } else {
                $key = $buff;
                $status = JSON_PARSE;
              }
            } else if ($curr == ',' || $curr == '}') {
              if ($key) {
                $json[$key] = $buff;
              } else {
                $json[] = $buff;
              }
              $key = null;
              $status = JSON_PARSE;
            } else {
              trigger_error("Got {$curr}, expecting delimiter.", E_USER_ERROR);
            }
          }
        }
      }
    }

    return $json;
  }

  public static function run_test() {

    $array = array(
      'foo' => array(
        'bar',
        'baz' => array(1, 2, 3, 4, 5, 6, 7),
        array(
          'a', 'b', 'c', 'd'
        )
      ),
      'bar' => array(5, 4, 3, 2, 1),
      'oop' => 'Object Oriented Programming',
      'baz' => array(
        1,2,3,4,5, array('a', array(1, 2, 3), 'b', 'x' => 'c')
      )
    );

    $json = new json();

    $string = $json->encode($array);

    print_r($string);

    $converted = $json->decode($string);

    print_r($converted);
  }
}

?>