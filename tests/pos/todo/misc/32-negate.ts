function plus(x:number, y:number):number {
    return plus(x, y); 
}

// If you leave the outputs unrefined, then rsc infers a refinement
// that mentions `x` but which is malformed when you do the substitution,
// unless the DEADCAST basically stops consgen from that point onwards i.e. 
// DEADCAST is treated as a return.

/*@ nein :: (x:boolean) => {v:boolean | Prop v <=> not (Prop x)} */
function nein(x){
    return !x;
}

/*@ negate :: /\ (x:number)  => {v:number | 0 < 1} 
              /\ (x:boolean) => boolean

*/
function negate(x):any {
  if (typeof(x) === "number") {
      return plus(x, 1);
  } 
  
  return nein(x);
  
}

/*@ foo :: (number) => {v:number | v = 1} */
function foo(x:number):any {
    return 1;
    return "cat";
}