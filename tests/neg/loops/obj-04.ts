
/*@ foo :: () => { a: { number | 40 = v } } */

function foo() {

  let x = { a: 1 };

  for (let i = 0; i < 5; i++) {
     x = { a: i };
  }

  return x;

}
