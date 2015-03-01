//adapted from transducers
class Bar { 
  constructor() {}
}
/*@ foo :: (IArray<number>) => void */
declare function foo(a);

declare var x:Bar;

if (false) { 
  foo(x); 
}
