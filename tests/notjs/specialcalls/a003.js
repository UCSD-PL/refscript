var foo = function(a,b) {return 2343};
var o = {foo: function(a,b) {return a*b;}};
var obj = {o: {foo: function(a,b) {return a-b;}}};

with (obj) {
	foo.call(o, 2, 2);
}
