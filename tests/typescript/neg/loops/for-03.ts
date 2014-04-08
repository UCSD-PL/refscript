
/*@ foo :: () => { number | v = 10 } */ 

function foo() {

  var x = 1;

  for (var i = 0; i < 5; i ++) {
     x = i;
  }

  return x;

}
