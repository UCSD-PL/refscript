
/*@ id :: forall A . (A,A) => A */
function id(a,b) {
  return b;
}

/*@ foo :: () => {a:number} */
function foo() {
  return id({a:2},{a:1});
}

