
/*@ readonly gobj :: # */
var gobj = { a: 5, b: "String", oo: { n: 6 } };

/*@ foo :: () => { n: { number | 0 < 1 } } */
function foo () { return gobj.oo; }

