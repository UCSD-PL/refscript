//adapted from transducers
class Bar { }
/*@ foo :: (IArray<number>) => void */
declare function foo(a);
declare var x:Bar;
if (false) { foo(x); }
