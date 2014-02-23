
/*@ foo :: forall A. (A, A) => A */
function foo(x:any, y:any):any {
  if (x == y) {
    return x;
  } else {
    return y;
  }
}

/*@ bar :: forall A. (A, A) => A */
function bar(x:any,y:any):any {
  return foo(x,y);
}
