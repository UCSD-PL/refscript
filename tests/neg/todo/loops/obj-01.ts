
/*@ foo :: () => { a: { number | v = 1 } } */ 

function foo() : Object {
 
  var x : Object = { a: 1 };
  
  var x : Object = { a: 1 };

  for (var i = 0; i < 5; i ++) {
     x = { a: 2 };
  }

  return x;

}
