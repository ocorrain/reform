
Fx.Elements=new Class({Extends:Fx.CSS,initialize:function(elements,options){this.elements=this.pass=$$(elements);arguments.callee.parent(options);},compute:function(from,to,delta){var now={};for(var i in from){var iFrom=from[i],iTo=to[i],iNow=now[i]={};for(var p in iFrom)iNow[p]=arguments.callee.parent(iFrom[p],iTo[p],delta);}
return now;},set:function(now){for(var i in now){var iNow=now[i];for(var p in iNow)this.render(this.elements[i],p,iNow[p]);}
return this;},start:function(obj){if(!this.check(obj))return this;var from={},to={};for(var i in obj){var iProps=obj[i],iFrom=from[i]={},iTo=to[i]={};for(var p in iProps){var parsed=this.prepare(this.elements[i],p,iProps[p]);iFrom[p]=parsed.from;iTo[p]=parsed.to;}}
return arguments.callee.parent(from,to);}});