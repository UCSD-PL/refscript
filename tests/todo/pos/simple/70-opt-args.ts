
/*@ foo :: () => {void | 0 < 1} */
/*@ foo :: (l: {number | 0 <= v}) => {void | 0 < 1} */
function foo(l?: number)
{

  /*@ local loc :: number */
  let loc = 0;

  if (arguments.length === 1) {
    loc = l;
  }

  assert(0 <= <number>loc);
}
