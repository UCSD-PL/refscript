var x = {"a": 1};
var y = {"a": lSLoV(true, "private")};

function foo() {
 if (1) return x;
 else return y;
}

var z = foo();
z = {};
z.a = "str";
y.a
