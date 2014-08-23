

/*@ fst :: forall M A B . (x: #Pair[M,A,B]) => A */
function fst<A,B>(p: Pair<A,B>): A {
  return p.x;
}

/*@ snd :: forall M A B . (x: #Pair[M,A,B]) => B */
function snd<A,B>(p: Pair<A,B>): B {
  return p.y;
}

/*@ o :: { z: string  } */
var o = { z : "ASDFGHJKL" }

var obj = snd({ x: 1, y: o});

var a = obj.z;

assert(a === "ASDFGHJKL");

