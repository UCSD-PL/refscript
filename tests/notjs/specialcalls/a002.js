//var f = function(a,b) {return a*b;};

var obj = {foo: function(a,b) {return a-b;}};

with (obj) {
	foo.call(obj, 2, 2);
}
