
function foo<A>(x: A, y: A): A {
  if (x === y) {
    return x;
  } else {
    return y;
  }
}

export function bar<A>(x: A,y: A): A {
  return foo(x,y);
}
