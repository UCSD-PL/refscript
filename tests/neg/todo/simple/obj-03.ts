
/*@ oooo :: { f: { number | v > 0 } } */
var oooo = { f: 1 };

oooo.f = -1;

assert(oooo.f > 0);

