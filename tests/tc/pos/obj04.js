
/*@ foo :: () => { } */
function foo() {
  var r = { };
  if (0<1) 
    r = { a: 1, b: true };
  else
    r = { a: true };

  return r;
}

