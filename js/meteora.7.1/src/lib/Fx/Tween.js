/*
Script: Fx.Tween.js
	Formerly Fx.Style, effect to transition any CSS property for an element.

License:
	MIT-style license.
*/

Fx.Tween = new Class({

	Extends: Fx.CSS,

	initialize: function(element, property, options){
		this.element = this.pass = $(element);
		this.property = property;
		arguments.callee.parent(options);
	},

	set: function(now){
		this.render(this.element, this.property, now);
		return this;
	},

	start: function(){
		var fromto = Array.slice(arguments);
		if (!this.check(fromto)) return this;
		var parsed = this.prepare(this.element, this.property, fromto);
		return arguments.callee.parent(parsed.from, parsed.to);
	}

});

Element.Properties.tween = {

	set: function(options){
		var tween = this.retrieve('tween');
		if (tween) tween.cancel();
		return this.store('tween', new Fx.Tween(this, null, $extend({link: 'cancel'}, options)));
	},

	get: function(property, options){
		if (options || !this.retrieve('tween')) this.set('tween', options);
		var tween = this.retrieve('tween');
		tween.property = property;
		return tween;
	}

};

Element.implement({

	tween: function(property){
		var tween = this.get('tween', property);
		tween.start.apply(tween, Array.slice(arguments, 1));
		return this;
	},

	fade: function(how){
		var fade = this.get('tween', 'opacity');
		how = $pick(how, 'toggle');
		switch (how){
			case 'in': fade.start(1); break;
			case 'out': fade.start(0); break;
			case 'show': fade.set(1); break;
			case 'hide': fade.set(0); break;
			case 'toggle': fade.start((function(){
				return (this.getStyle('visibility') == 'hidden') ? 1 : 0;
			}).bind(this)); break;
			default: fade.start.apply(fade, arguments);
		}
		return this;
	},

	highlight: function(start, end){
		if (!end){
			var style = this.getStyle('background-color');
			end = (style == 'transparent') ? '#ffffff' : style;
		}
		this.get('tween', 'background-color').start(start || '#ffff88', end);
		return this;
	},

	effect: function(property, options){
		return new Fx.Tween(this, property, options);
	}

});
