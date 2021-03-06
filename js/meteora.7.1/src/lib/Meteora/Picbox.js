/**
 * Picbox
 * ---
 * Written by José Carlos Nieto <xiam@astrata.com.mx>
 *
 * Copyright (c) 2008-2009 Astrata Software S.A. de C.V.
 *
 * Licensed under The MIT License
 * Redistributions of files must retain the above copyright notice.
 *
 * @author          José Carlos Nieto
 * @copyright       Copyright (c) 2009-2009, Astrata Software S.A. de C.V.
 * @link            http://astrata.com.mx Astrata Open Source Projects
 * @version         $Revision: $
 * @modifiedby      $LastChangedBy: $
 * @lastmodified    $Date: $
 * @license         http://www.opensource.org/licenses/mit-license.php The MIT License
 *
 */

var PicboxImg = new Class({
  
  'Implements': [ Control ],
  
  'close': function() {

    this.photo = null;
    
    Meteora.removeOverlay();

    Picbox.active = null;
  },
  
  'previous': function() {

    if (this.isLoading == false) {

      var imgs = $(document.body).getElementsByClassName('picbox-photo');

      if (imgs) {

        var prev = null;
        var first = null;
        var found = false;

        // first pass
        for (var i = imgs.length - 1; prev == null && i >= 0; i--) {
          var curr = imgs[i];
          if (curr.parentNode.nodeName.toLowerCase() == 'a') {
            if (!first) {
              first = curr;
            }
            if (this.element == curr) {
              found = true;
            } else if (found) {
              if (this.photo != curr) {
                prev = curr;
              }
            }
          }
        }

        if (prev == null) {
          prev = first;
        }
        
        if (prev) {
          this.load(prev);
        }
      }
    }
    
  },

  'next': function() {

    if (this.isLoading == false) {

      var imgs = $(document.body).getElementsByClassName('picbox-photo');

      if (imgs) {

        var next = null;
        var first = null;
        var found = false;

        // first pass
        for (var i = 0; next == null && i < imgs.length; i++) {
          var curr = imgs[i];
          if (curr.parentNode.nodeName.toLowerCase() == 'a') {
            if (!first) {
              first = curr;
            }
            if (this.element == curr) {
              found = true;
            } else if (found) {
              if (this.photo != curr) {
                next = curr;
              }
            }
          }
        }

        if (next == null) {
          next = first;
        }
        
        if (next) {
          this.load(next);
        }
      }
    }
    
  },

  'load': function(el) {

    this.isLoading = true;

    var anchor = el.parentNode;
    
    this.photo.dump();

    this.element.store('photo', null);

    this.element = $(el);
    this.photo = Widget.img(); 
    this.element.store('photo', this.photo);

    this.legend.hide();
    this.loading.show();
    this.photo.onload = this.onLoad.bind(this);

    this.photo.src = anchor.href;
  },

  'onLoad': function() {

    this.isLoading = false;

    this.photo.onload = function() {};

    var maxH = Browser.clientHeight()*0.7;
    var maxW = Browser.clientWidth()*0.7;

    this.photo.onclick = this.next.bind(this);
    this.photo.style.cursor = 'pointer';

    var factor = 1;

    if (this.photo.width > maxW) {
      factor = maxW/this.photo.width;
    }
    
    if (this.photo.height > maxH) {
      factor = maxH/this.photo.height;
    }

    var nw = this.photo.width*factor;
    var nh = this.photo.height*factor;

    this.photo.width = nw;

    if (Browser.Engine.trident) {
      this.photo.height = nh;
    }

    this.dialog.components.dialog.style.position = 'absolute';

    var x = Browser.pageScrollX()+((Browser.clientWidth() - nw)/2);
    var y = Browser.pageScrollY()+((Browser.clientHeight() - nh)/2);

    Effect.left(this.dialog.components.dialog, x);
    Effect.top(this.dialog.components.dialog, y);
    
    this.dialog.resizeTo(nw+50, nh+60, true);

    window.setTimeout(
      function() {
        
        this.loading.hide();
        this.photo.hide();
        this.frame.insertBefore(this.photo, this.legend);

        Effect.appear(this.photo);
        Effect.appear(this.legend);

      }.bind(this), 400
    );

    Picbox.active = this;
  },

  'reload': function() {
    this.load(this.element);
  },

  'initialize': function(el) {

    this.element = $(el);

    if (el.retrieve('photo')) {
      $(el.retrieve('photo')).dump();
    }

    var anchor = el.parentNode;

    this.photo = Widget.img();

    el.store('photo', this.photo);

    this.loading = Widget.img({'src': $meteora['mediaDir']+'/core/loading.gif'});

    this.legend = Widget.div(null, Widget.small(null, Widget.fromHTML('<span style="white-space: nobreak;">'+__('Keyboard shortcuts: escape <i>exit</i>, left <i>previous photo</i>, right <i>next photo</i>')+'</span>')));
   
    this.frame = Widget.div({
        'style': {'padding': '8px', 'textAlign': 'center'}
      },
      [ this.loading, this.legend ]
    );

    this.legend.hide();

    this.dialog = new Dialog(
      this.frame,
      {
        'title':  el.title ? el.title : anchor.href.replace(/.*\//, ''),
        'width':  100,
        'height': 100,
        'onClose': this.close
      }
    );

    var coords = this.element.getCoordinates();

    //this.dialog.show();
    
    this.dialog.moveTo(coords.top, coords.left);
    this.dialog.resizeTo(coords.width, coords.height);
    
    Effect.appear(this.dialog.components.dialog);

    Meteora.overlay({
      'onClick': this.close.bind(this)
    });
    
    this.isLoading = true;
    
    this.photo.onload = this.onLoad.bind(this);

    this.photo.src = anchor.href;
  }
});

var Picbox = new Class({
  
  'Implements': [ Control ],

  '__documentKeydown': function(e) {
    e = new Event(e);

    var active = Picbox.active;

    if (active) {
      switch (e.key) {
        case 'esc':
          active.close();
          e.stop();
        break;
        case 'up':    case 'left':
          active.previous(); 
          e.stop();
        break;
        case 'down':  case 'right':
          active.next();
          e.stop();
        break;
      }
    }
  },

  '__documentClick': function(e) {
    var e = new Event(e);
    if (!e.rightClick) {
      if ($type(e.target) == 'element') {
        if (e.target.nodeName.toLowerCase() == 'img') {
          if (e.target.parentNode && e.target.parentNode.nodeName.toLowerCase() == 'a' && (e.target.parentNode.href.match(/\.(jpg|jpeg|gif|png|bmp)$/i) || e.target.className == 'picbox-photo')) {
            e.stop();
            new PicboxImg(e.target);
            return false;
          }
        }
      }
    }
  },

  '__windowResize': function() {
    if (Picbox.active) {
      Picbox.active.reload();
    }
  },

  '__bindEvents': function() {
    this.addListener(
      document,
      'click',
      this.__documentClick
    );

    this.addListener(
      document,
      'keydown',
      this.__documentKeydown
    );

    this.addListener(
      window,
      'resize',
      this.__windowResize
    );
  },

  'initialize': function() {
    Picbox.active = null;
    this.__bindEvents();
  }
});

Meteora.onStart(
  function() {
    new Picbox();
  }
);

