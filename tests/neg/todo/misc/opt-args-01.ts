
/*@ foo :: /\ () => {void | 0 < 1}            
           /\ (l: {number | 0 <= v}) => {void | 0 < 1} */
function foo(l?: number) 
{

  /*@ local loc :: number + undefined */
  var loc = l;

  if (arguments.length < 1) {

    /*@ local zero :: number + undefined */
    var zero = -1;
    loc = zero;
  }

  var l1 = <number>loc;

  assert(0 <= l1);
}
