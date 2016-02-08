
/*@ foo :: () => number */
export function foo() {
  let i = 0;
  let z: number | string = 10;

  while (i < 5) {
     i = i + 1;
     z = "dog"; // whoops, should be same as outside.
  }

  return z;
}
