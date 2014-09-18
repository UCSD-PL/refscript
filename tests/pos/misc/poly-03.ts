

function idp<A>(x: A): A {
  return x; 
}

/*@ bar :: () => { number | v = 5 }*/
function bar(): number {
  var f = <(number) => number> idp;
  return f(5);
}


