var x = lSLoV({}, "private");
x.a = 1;
x.foo = function() { return this.a; };

var z = x.foo();
z


