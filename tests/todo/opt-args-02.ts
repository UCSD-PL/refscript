
/*@ foo :: /\ () => {void | 0 < 1}            
           /\ (l: {number | 0 <= v}) => {void | 0 < 1} */
function foo(l?: number) 
{

  /*@ local loc :: number */
  var loc = 0;

  if (arguments.length === 1) {
    loc = l;
  }

  assert(0 <= loc);
}
