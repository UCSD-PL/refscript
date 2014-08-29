

interface Pair<A,B> {
  x: A;
  y: B;
}

/*@ fst :: forall A B . (x: #Pair[A,B]) => A */
function fst<A,B>(p: Pair<A,B>): A {
  return p.x;
}

/*@ snd :: forall A B . (x: #Pair[A,B]) => B */
function snd<A,B>(p: Pair<A,B>): B {
  return p.y;
}

var o = { z : "ASDFGHJKL" }

var obj = snd({ x: 1, y: o});

var a = obj.z;

assert(a === "ASDFGHJK");

