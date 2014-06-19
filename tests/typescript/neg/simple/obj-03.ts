
/*@ gobj :: { a: { number | v > 0 } } */
var gobj = {
  a: 1
};

/*@ foo :: () => { void | true } */
function foo() {
  gobj.a = gobj.a - 1;
}

