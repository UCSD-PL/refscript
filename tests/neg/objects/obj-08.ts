
/*@ a :: { b: { c: { number | v > 0 } } } */
var a  = { b : { c: 3 } };


/*@ foo :: (o: []{ c: number }) => void */
function foo (o: { c: number }) { 

  o.c = 1;

}

foo(a.b);


assert(a.b.c > 0);

