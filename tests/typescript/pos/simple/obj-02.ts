
/*@ foo :: () => void */
function foo() {}

var a = { f1: 1, f2: "s", f3: true, f4: foo };

a.f1 = 1;

var c = a;

/* b :: { f1: { number | v > 0 } } */
var b = { f1: 2, f2: "s", f3: true, f4: foo };

b.f1 = 2;

var d = b


assert(d.f1 > 1);
