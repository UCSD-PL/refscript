

// XXX: This is super ugly -- FIX !!! 

/*@ a :: [#Mutable]{ b: [#Mutable] [#Mutable]{ c: [#Mutable] number } } */
var a  = { b : { c: 3 } };


/*@ foo :: (o: [#Mutable]{ c: [#Mutable] number }) => void */
function foo (o: { c: number }) { 

  o.c = 1;

}

foo(a.b);


// assert(a.b.c > 0);
