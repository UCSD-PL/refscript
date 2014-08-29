
/*@ foo :: () => { a: { number | v = 1 } } */ 

function foo() {


  var x = { a: 1 };

  for (var i = 0; i < 5; i ++) {
     x = { a: 2 };
  }

  return x;

}
