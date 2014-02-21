var gobj = {
  a: 5
};

/*@ foo :: () => { number | v > 0 } */
function foo () {
  var z = gobj.a;
  return z;
}
