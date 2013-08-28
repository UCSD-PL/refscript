function foo(x) {
  this.a = x;
}

var p = new foo("baz");
var q = new foo("bar");

q.a;
