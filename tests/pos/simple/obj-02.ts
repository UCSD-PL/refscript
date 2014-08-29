
/*@ foo :: () => void */
function foo() {}

var a = { f1: 1, f2: "s", f3: true, f4: foo };

a.f1 = 1;

var c = a;

/*@ b :: [#Mutable]{ f1: { number | v > 1 }; f3: boolean } */
var b = { f1: 2, f2: "s", f3: true, f4: foo };

b.f3 = false;

var d = b

assert(d.f1 > 1);
