
/*@ a :: posint */
let a = 1;
let b = a + 1;
a = a + 1;
let c = a + 1;
assert(b === c);
