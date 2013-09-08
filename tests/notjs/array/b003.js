
/*@ foo :: ( { v: number| v > 0} ) => void */
function foo (n) {

  var a = []; // --> [T]  type parameter needs instantiation 
              // --  what if not instantiated? -> make undefined

  a[0] = n; 

}
