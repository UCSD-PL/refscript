
/*@ foo :: /\ () => {void | 0 < 1}            
           /\ (l: {number | 0 <= v}) => {void | 0 < 1} */
function foo(l?: number) 
{
  /*@ local l1 :: number */
  var l1 = (arguments.length < 1) ? 0 : l;
  assert(0 <= l1);
}
