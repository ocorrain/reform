/**
 * Toolbox
 * A floating div to handle tools
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

var Toolbox = new Class({

  'Implements': [ Control ],
  
  'options': {
    'autoClose':    true,
    'autoMove':     false,
    'enableDrag':   true
  },

  'initialize': function(el, options) {

    this.setMainElement(el);

    this.setOptions(options);

    this.__buildComponents();
    this.__bindEvents();

  },

  '__buildComponents': function() {
    
    this.components = {
      'frame': Widget.div({ 'class': 'm-toolbox' }, this.element)
    }; 
    
    this.hide();

    document.body.appendChild(this.components.frame);
  },


  '__bindEvents': function() {

    this.addListener(
      this.components.frame,
      'mouseup',
      this.__frameMouseUpEvent
    );

    this.addListener(
      document,
      'mousedown',
      this.__documentMouseDownEvent
    );

    if (this.options.enableDrag) {
      this.components.frame.makeDraggable();
    }
  },

  'followMouse': function(e) {
    this.show();
    this.moveTo(Browser.pageScrollY()+e.client.y+10, Browser.pageScrollX()+e.client.x);
    this.components.frame.setOnTop();
  },

  '__documentMouseDownEvent': function(e) {

    var parent = e.target;

    while (parent) {
      if (parent == this.components.frame) {
        return;
      }
      if (parent == this.options.assocObject) {
        this.show();    
        return;
      }
      parent = parent.parentNode;
    }
    if (this.options.autoClose) {
      this.hide();
    }
  },

  '__frameMouseUpEvent': function(e) {
    var el = this.components.frame;
    if (this.options.autoMove) {
      el.setStyles({
        'position': 'absolute',
        'top':      e.client.y+'px',
        'left':     e.client.x+'px'
      });
      this.show();
    }
  },

  'hide': function() {
    this.components.frame.hide();
  },

  'show': function() {
    this.components.frame.show();
    this.components.frame.setOnTop();
  },
  
  'setContent': function(el) {
    this.element.setContent(el);
  },

  'resizeTo': function(width, height) {
    this.components.frame.setStyles({
      'width':  width+'px',
      'height': height+'px'
    });
  },

  'moveTo': function(x, y) {
    this.components.frame.moveTo(x, y);

    // Repositioning
    var el = this.components.frame;

    var coord = el.getCoordinates();

    var offset = coord.top + coord.height - ( Browser.pageScrollY()+Browser.clientHeight() );
    
    if (offset > 0) {
      el.style.top = (coord.top - offset - 30) + 'px';
    }

    var offset = coord.left + coord.width - ( Browser.pageScrollX()+Browser.clientWidth() );
    if (offset > 0) {
      el.style.left = (coord.left - offset - 30) + 'px';
    }
    
    // Is window too small?
    var coord = el.getCoordinates();
    if (coord.top < 0) {
      el.style.top = (Browser.pageScrollY()+10)+'px';
    }

    if (coord.left < 0) {
      el.style.left = (Browser.pageScrollX()+10)+'px';
    }
  },

  'isHidden': function() {
    return (this.components.frame.style.display == 'none');
  }
});
