
/*@ foo :: () => { a: { number | v = 1 } } */
function foo() : Object {

    /*@ local x :: (ReadOnly) { a: number } */
  let x = { a: 1 };

  for (let i = 0; i < 5; i ++) {
     x = { a: 1 };
  }

  return x;

}
