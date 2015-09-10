
interface Pair1<A> {
  x:A;
  y:A;
}

interface ColorPair1<A,C> extends Pair1<A> { 
  c:C;
}

/*@ foo :: forall M A . (Pair1<M, A>) => void */
function foo<A>(p: Pair1<A>) {

}

/*@ p :: ColorPair1<Immutable,number,string> */
declare var p: ColorPair1<number, string>;

foo(p);

assert(typeof p.x === "string");

