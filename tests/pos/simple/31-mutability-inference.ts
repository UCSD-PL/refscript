
export let a = { f1: 1, f2: "s", f3: true };
a.f1 = 1;

let c = a;

/*@ b :: { f1: { number | v > 1 }; f3: [Mutable] boolean; } */
let b = { f1: 2, f2: "s", f3: true };
b.f3 = false;

let d = b
assert(d.f1 > 1);
