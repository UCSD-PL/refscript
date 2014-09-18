

function idp<A>(x: A): A {
  return x; 
}

/*@ bar :: forall T . () => { number | true } */
function bar<T>(): any {
  var f = <(T) => T> idp;
  return f(5);
}


