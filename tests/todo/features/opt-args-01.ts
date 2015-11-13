
/*@ foo :: /\ () => {void | 0 < 1}            
           /\ (lo: {number | 0 <= v}) => {void | 0 < 1} */
function foo(lo?: number) 
{
  if (arguments.length < 1)
    lo = 0;

  var lo1 = <number>lo;

  assert(0 <= lo1);
}
