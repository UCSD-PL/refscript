
/*@ foo :: /\ () => {void | true}            
           /\ (l: {number | 0 <= v}) => {void | true} */
function foo(l?: number) 
{

  /*@ local loc :: number + undefined */
  var loc = l;

  if (arguments.length < 1) {

    /*@ local zero :: number + undefined */
    var zero = 0;
    loc = zero;
  }

  var l1 = <number>loc;

  assert(0 <= l1);
}
