

function idp<A>(x: A): A {
  return x; 
}

/*@ bar :: () => { number | v = 6 }*/
function bar(): number {
  var f = <(number) => number> idp;
  return f(5);
}


