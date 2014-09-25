
interface Point<A1> {
  x:A1;
  y:A1;
}

interface ColorPoint<A2,C2> extends Point<A2> { 
  c:C2;
}

/*@ foo :: forall M A3 . (#Point<M,A3>) => void */
function foo<Z>(p: Point<Z>) {
    return;
}

/*@ p :: #ColorPoint[#Immutable,number,string] */
var p: ColorPoint<number, string>;

foo(p);

