
/*@ foo :: () => void */
function foo() {}

var a = { f1: 1, f2: "s", f3: true, f4: foo };

a.f1 = 1;



/*@ c :: [#ReadOnly] {  } */
var c = a;

c.f1 = 1;

