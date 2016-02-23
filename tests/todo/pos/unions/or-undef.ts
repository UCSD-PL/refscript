

// PV: needs sort-checking at RSC, when dropping the refinement from
//     then top-level to parts of then union.


/*@ check_undefined :: <T>(x: { T + undefined | v = y}, y: { T + undefined | v = x }) => T */
function check_undefined<T>(x:any, y: any) : T {
  if (typeof x === "undefined")
	  return crash();
  return <T>x;
}

check_undefined(1,1);
