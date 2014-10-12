
/*@ check_undefined :: forall T . (x: { T + undefined | v = y}, y: { T + undefined | v = x }) => T */
function check_undefined<T>(x:any, y: any) : T {
  if (typeof x === "undefined") 
	  return crash();
  return <T>x;
}

check_undefined(1,1);
