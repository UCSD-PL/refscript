/*@ foo :: () => List<Unique,{v:number| 10 < v}> */
function foo(){
  return { data: 10, next: null };
}

