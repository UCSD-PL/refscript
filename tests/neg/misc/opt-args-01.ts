
/*@ foo :: /\ () => {void | true}            
           /\ (lo: {number | 0 <= v}) => {void | true} */
function foo(lo?: number) 
{
  if (arguments.length < 1)
    lo = 0;

  var lo1 = <number>lo;

  assert(0 < lo1);
}
