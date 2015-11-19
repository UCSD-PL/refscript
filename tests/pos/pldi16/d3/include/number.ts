/// <reference path="d3.d.ts" />

/*@ d3_number :: (x:number + undefined) => {v:boolean | Prop(v) => ttag(x) == "number"} */
function d3_number(x:any):any{
  return x !== null && !isNaN(x);
}
