
/*@ foo :: () => { a: { number | 40 = v } } */ 

function foo() {

  var x = { a: 1 };

  for (var i = 0; i < 5; i++) {
     x = { a: i };
  }

  return x;

}
