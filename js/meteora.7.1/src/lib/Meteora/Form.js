/**
 * Form Control
 * ---
 *  Written by José Carlos Nieto Jarquín <xiam@astrata.com.mx>
 *
 * Copyright (c) 2007-2009 Astrata Software S.A. de C.V.
 *
 * Licensed under The MIT License
 * Redistributions of files must retain the above copyright notice.
 *
 * @author          José Carlos Nieto Jarquín
 * @copyright       Copyright (c) 2007-2009, Astrata Software S.A. de C.V.
 * @link            http://astrata.com.mx Astrata Open Source Projects
 * @version         $Revision: $
 * @modifiedby      $LastChangedBy: $
 * @lastmodified    $Date: $
 * @license         http://www.opensource.org/licenses/mit-license.php The MIT License
 *
 */

var Form = new Class({
  
  'Implements': [ Control ],
  
  '__iframeLoad': function() {

    window.setTimeout(this.unlock.bind(this), 500);

    var iframe = this.components.iframe;

    if (iframe.contentDocument) {
      var doc = iframe.contentDocument;
    } else if (iframe.contentWindow) {
      var doc = iframe.contentWindow.document;
    } else {
      var doc = iframe.document;
    }

    var response = doc.body.innerHTML;

    var tmp = Widget.textarea();
    
    tmp.innerHTML = response.replace(/</g, '&lt;').replace(/>/g, '&gt;');

    if (tmp.value) {

      if (tmp.value.match(/<pre/ig)) {
        tmp.value = tmp.value.replace(/<pre[^>]*?>/i, '').replace(/<\/pre>/i, '');
      }
      debug('RESPONSE', tmp.value);
      
      var rpc = new JsonRpc(tmp.value, this.element);
    }

    window.setTimeout(this.destroy.bind(this), 1000);
  },

  'submit': function() {
    this.element.submit();
  },
  'lock': function() {
    if (!this.element.className.match(/no-lock/)) {
      for (var i = 0; i < this.element.elements.length; i++) {
        var input = $(this.element.elements[i]);
        input.setAttribute('disabled', 'disabled');
      }
      this.__createMask();
    }
  },
  'unlock': function() {
    if (!this.element.className.match(/no-lock/)) {
      for (var i = 0; i < this.element.elements.length; i++) {
        var input = $(this.element.elements[i]);
        input.removeAttribute('disabled');
        input.disabled = false;
        if (input.bubble) {
          input.bubble.destroy();
          input.bubble = null;
        }
      }
      this.__removeMask();
    }
  },
  '__createMask': function() {
    if (Meteora.showLoading) {
      var coord = this.element.getCoordinates();
      if (coord['width'] && coord['height']) {
        this.__mask = Widget.div({'class': 'm-form-mask',
            'style': {
              'top': coord.top+'px',
              'left': coord.left+'px',
              'width': coord.width+'px',
              'height': coord.height+'px',
              'lineHeight': coord.height+'px'
            }
          },
          Widget.img({'src': $meteora['mediaDir']+'/core/loading.gif'})
        );
        this.__mask.setOpacity(0.8);
        document.body.appendChild(this.__mask);
      }
    }
  },
  '__removeMask': function() {
    if (this.__mask) {
      this.__mask.remove();
    }
  },
  'validate': function() {

    // validating form
    var elements = this.element.elements;
    var errors = false;
    var radios = {};

    for (var i = 0; i < elements.length; i++) {

      var parent = elements[i];
     
      var input = parent;
      
      if (input.bubble && input.bubble.element) {
        input.bubble.destroy();
      }
      
      input.bubble = null;

      while (parent) {

        if (parent.className && parent.className.match(/(required|optional)-?(\w+)?/) && parent.nodeName.toLowerCase().match('^(label|input|select|textarea)$')) {
          
          var test    = ' '+parent.className+' ';
          var message = '';
          var value   = input.value;
  
          var patterns = {
            'not-null': /.+/,
            'int':      /^-?\d+$/,
            'float':    /^-?(\d+|\d+\.\d+|\.\d+)$/,
            'double':   /^-?(\d+|\d+\.\d{2}|\.\d{2})$/,
            'alpha':    /^[a-z0-9\._+\-]+$/i,
            'email':    /^[a-z0-9\._+\-]+@[a-z0-9\.\-]+$/i
          }

          var match = test.match(/\s(required|optional)-?(\w+)?\s/);
            
          if (match) {

            if (input.type == 'radio') {

              if (!radios[input.name]) {
                var isChecked = false;
                for (j = 0; j < elements.length; j++) {
                  var test = elements[j];
                  if (test.type == 'radio' && test.name == input.name) { 
                    if (test.checked) {
                      isChecked = true;
                    }
                  }
                }
                if (!isChecked && match[1] == 'required') {
                  message = __('Please, select at least one option.');
                }
                radios[input.name] = true;
              }
            
            } else if (input.type == 'checkbox') {
           
              if (!input.checked && match[1] == 'required') {
                message = __('You must check this input in order to continue submitting this form.');
              }

            } else {

              var correct = false;
              
              if (typeof match[2] == 'undefined') {
                match[2] = 'not-null';
              }

              if (typeof patterns[match[2]] != 'undefined') {
                var pattern = patterns[match[2]];
                if (value.match(pattern)) {
                  correct = true;
                }
              } else {
                // console.log('unknown pattern.'+match[2]);
              }

              if (match[1] == 'optional' && !value) {
                correct = true;
              }

              if (!correct) {
                switch (match[2]) {
                  case 'not-null':
                    message = __('You cannot leave this field empty.');
                  break;
                  case 'int':
                    message = __('This field should contain an integer number.');
                  break;
                  case 'double':
                    message = __('This field should contain a double decimal number.');
                  break;
                  case 'float':
                    message = __('This field should contain a floating point number.');
                  break;
                  case 'alpha':
                    message = __('This field should contain letters, dot, underscore or plus/minus signs.');
                  break;
                  case 'email':
                    message = __('This field should contain an e-mail address.');
                  break;
                }
              }
            }
          }

          if (message) {
            while (parent) {
              if (parent.style && parent.style.display == 'none') {
                break;
              }
              parent = parent.parentNode;
            }
            if (parent == null) {
              input.bubble = new Bubble(
                input,
                message,
                {
                  'position':     'middle right',
                  'showEvent':    'focus',
                  'hideEvent':    'blur',
                  //'destroyEvent': 'blur',
                  'width':        350,
                  'height':       40
                }
              );
              input.bubble.red();
              input.bubble.show();
              errors = true;
            }
          }
        }
        if (parent) {
          parent = parent.parentNode;
        }
      }
    }
    return errors ? false : true;
  },
  '__onSubmit': function() {

    if (this.element._onsubmit) {
      var result = this.element._onsubmit.call(this.element);
      if (result == false) {
        return false;
      }
    }

    if (this.validate()) {

      // Automatic variable
      if (!this.element.__ajax) {
        this.element.__ajax = Widget.input({'style': {'display': 'none'}, 'name': '__ajax', 'value': 1});
        this.element.appendChild(this.element.__ajax);
      }

      // Searching for an unused iframe
      for (var i = 0; $('__meteora_aux_'+i); i++); 
      
      // Setting iframe's id
      var target = '__meteora_aux_'+i;
      
      // If for some reason the id already exists we need to remove it
      if ($(target)) {
        $(target).dump();
      }

      var iframe = Widget.iframe({
        'id':   target,
        'name': target,
        'src':  Browser.Engine.presto ? 'opera:about' : 'about:blank'
      });

      iframe.hide();

      document.body.appendChild(iframe);

      // Waiting for the body to append this child
      while (!$(target));

      var iframe = $(target);

      this.components = {
        'iframe': iframe
      }

      this.addListener(
        this.components.iframe,
        'load',
        this.__iframeLoad
      );

      this.element.setAttribute('target', target);

      log(this.element.method.toUpperCase()+' '+this.element.action);

      var post = [];
      for (var i = 0; i < this.element.elements.length; i++) {
        var el = this.element.elements[i];
        var x = {};
        x[el.name] = el.value;
        post.push(x);
      }
      
      debug('DATA', post);

      this.submit();

      window.setTimeout(this.lock.bind(this), 500);

    }
    
    return false;

  },

  'initialize': function(form) {
    this.element = $(form);
  }
});

if (typeof(formSubmit) == 'undefined') {
  formSubmit = function(el) {
    var form = new Form(el);
    return form.__onSubmit();
  }
}

