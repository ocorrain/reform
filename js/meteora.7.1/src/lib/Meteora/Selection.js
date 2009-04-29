/**
 * Selection
 * ---
 *  Written by José Carlos Nieto Jarquín <xiam@astrata.com.mx>
 *  Copyright (c) 2007-2009 Astrata Software S.A. de C.V.
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

var Selection = new Class({
  
  'Implements': [ Control ],

  '__offset': 20,

  '__elementClick': function(e) {

    var e = new Event(e);

    var parent = e.target;

    while (parent) {

      if (parent.className == this.options.itemClass) {

        parent.__origParent.appendChild(parent);
        parent.__origParent = null;
    
        this.__checkElements();

        return;
      }
      parent = parent.parentNode;
    }
  },
  
  '__elementMouseOver': function(e) {
    var e = new Event(e);
    var parent = e.target;

    while (parent) {
      if (parent.className == this.options.itemClass) {
        parent.setOnTop();
      }
      parent = parent.parentNode;
    }
  },

  '__documentMouseDown': function(e) {

    e = new Event(e);

    var parent = e.target;
    
    if (e.rightClick == false) {

      document.onselectstart = function() {
        return false;
      }

      var insideArea = false;
       
      if (parent == this.element) {
        return;
      }

      while (parent) {
        if (parent == this.components.selectionArea) {
          insideArea = true;
          break;
        }
        if (parent == this.element) {
          return;
        }
        parent = parent.parentNode;
        return;
      }
      
      if (insideArea) {

        e.preventDefault();

        if (this.__documentMouseMoveListener) {
          this.removeListener(this.__documentMouseMoveListener);
        }

        this.__documentMouseMoveListener = this.addListener(
          document,
          'mousemove',
          this.__documentMouseMove
        );

        this.element.setStyle('position', 'absolute');

        this.element.__origX = e.page.x;
        this.element.__origY = e.page.y;

        this.element.setStyles(
          {
            'left': this.element.__origX+'px',
            'top':  this.element.__origY+'px'
          }
        );

        this.element.setStyles({
          'width': '0px',
          'height': '0px'
        });

        for (var i = 0; i < this.element.childNodes.length; i++) {
          var node = this.element.childNodes[i];
          if ($type(node) == 'element') {
            $(node).hide();
          }
        }

        this.element.show();
      }
    }
  },
  
  '__documentMouseMove': function(e) {

    var e = new Event(e);

    var height  = e.page.y-this.element.__origY;
    var width   = e.page.x-this.element.__origX;

    if (height < 0) {
      this.element.setStyle('top', (this.element.__origY+height+10)+'px');
    } else {
      height -= 10;
    }
    if (width < 0) {
      this.element.setStyle('left', (this.element.__origX+width+10)+'px');
    } else {
      width -= 10;
    }
    
    this.element.setStyles({
      'height': Math.abs(height)+'px',
      'width':  Math.abs(width)+'px'
    });

  },

  '__documentMouseUp': function(e) {

    this.removeListener(this.__documentMouseMoveListener);

    document.onselectstart = function() {
      return true;
    }

    var parent = e.target;

    while (parent) {
      if (parent == this.components.selectionArea) {
        break;
      }
      if (parent == this.element) {
        return;
      }
      parent = parent.parentNode;
    }

    var coord = this.element.getCoordinates();
    var elements = [];
    elements.extend($(document.body).getElementsByClassName(this.options.itemClass));
    var children = [];

    while (elements.length) {
      var element = $(elements.pop());
      var subcoord = element.getCoordinates();
      if (subcoord.top >= coord.top && subcoord.left >= coord.left && subcoord.right <= coord.right && subcoord.bottom <= coord.bottom) {
        if (element.parentNode != this.element) {
          element.__origParent = element.parentNode;
          this.element.appendChild(element);
        }
      }
    }


    this.__checkElements();
  
  },

  '__checkElements': function() {
    var elements = 0;
    var child = null;
    for (var i = 0; i < this.element.childNodes.length; i++) {
      var child = this.element.childNodes[i];
      if ($type(child) == 'element') {
        child.setStyles({
          'zIndex':   0,
          'position': 'absolute',
          'top':      (i*this.__offset)+'px',
          'left':     (i*this.__offset)+'px'
        });
        child.show();
        elements++;
      }
    }
    var coord = this.element.getCoordinates();
    if ($type(child) == 'element') {
      var corner = child.getCoordinates();
      this.element.setStyles({
        'width':  (corner.right - coord.left + this.__offset)+'px',
        'height': (corner.bottom - coord.top + this.__offset)+'px'
      });
    }
    if (elements == 0) {
      this.element.hide();
    }
  },

  '__selectItem': function(e) {
    e = new Event(e);
    var t = e.target;
    while (t) {
      if (t.className == this.options.itemClass) {
        t.setOnTop();
        return;
      }
      t = t.parentNode;
    }
  },

  '__bindEvents': function() {

    this.addListener(
      this.element,
      'mouseover',
      this.__elementMouseOver
    );
    
    this.addListener(
      this.element,
      'click',
      this.__elementClick
    );

    this.addListener(
      document,
      'mousedown',
      this.__documentMouseDown
    );

    this.addListener(
      document,
      'mouseup',
      this.__documentMouseUp
    );
    
  },
  
  '__buildComponents': function() {
    
    this.components = {
      'selectionArea': $(this.options.selectionArea)
    }

    this.element = Widget.div({
      'class':    'm-selection'
    });

    this.element.setOpacity(0.9);

    this.element.hide();

    document.body.appendChild(this.element);

    this.components.dropArea = [];

    for (var i = 0; i < this.options.dropArea.length; i++) {
      this.components.dropArea[i] = $(this.options.dropArea[i]);
      this.components.dropArea[i].parent = this;
      this.components.dropArea[i].addEvent(
        'drop',
        function(el) {
          if (el.className == 'm-selection') {
            this.parent.__onDrop(this);
          }
        }
      );
    }

    new Drag.Move(
      $(this.element),
      {
        'limit': {x: [0], y: [0]},
        'droppables': this.components.dropArea
      }
    );
  },
  
  '__onDrop': function(el) {
    var s = [];
    for (var i = 0; i < this.element.childNodes.length; i++) {
      var node = this.element.childNodes[i];
      if (node.className == this.options.itemClass) {
        node.__clickHandler = function(e) {
          var e = new Event(e);
          var node = e.target;
          this.components.selectionArea.appendChild(node);
          e.target.removeEvent(
            'click',
            node.__clickHandler
          );
        }.bindWithEvent(this);
        node.addEvent(
          'click',
          node.__clickHandler
        );
        s.push(node);
      }
    }
    if (this.$events.onDrop) {
      this.fireEvent('onDrop', [ s, el ]);
    }
    this.element.dumpChildren();
    this.element.hide();
  },

  'initialize': function(options) {
    
    this.setOptions(options);

    this.__buildComponents();
    this.__bindEvents();
  }

});
