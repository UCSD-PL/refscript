
/*@ check_undefined :: forall T.(T + undefined) => {T | 0 < 1} */
function check_undefined<T>(x:any) : T{
    if (typeof x === "undefined") 
	return crash();
    
    // OK    
    // return x;

    // FAILS 
    return <T>x;
    // PROBABLY "parses" the above as "TApp T []" ... aha.
}
  
 