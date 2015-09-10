
/*@ readonly gobj :: # */
var gobj = { a: 5, b: "String", oo: { n: 6 } };

/*@ foo :: () => { n: { number | true } } */
function foo () { return gobj.oo; }

