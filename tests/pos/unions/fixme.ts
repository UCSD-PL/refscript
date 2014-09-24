
/*@ check_undefined :: forall T.(T + undefined) => {T | true} */
function check_undefined<T>(x:any) : T{
    if (typeof x === "undefined") 
	return crash();
    return <T>x;
}
  
 