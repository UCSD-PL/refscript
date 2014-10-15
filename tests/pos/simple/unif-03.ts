
interface Point<A> {
  x:A;
  y:A;
}

interface ColorPoint<A,C> extends Point<A> { 
  c:C;
}

/*@ foo :: forall M A . (#Point[M, A]) => void */
function foo<A>(p: Point<A>) {

}

/*@ p :: #ColorPoint[#Immutable,number,string] */
declare var p: ColorPoint<number, string>;

foo(p);

