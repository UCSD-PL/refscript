
/*@ foo :: forall A . ([ [ number ] ], number ) => { [ [ number ] ] | true } */

function foo(a : number[], e : number) : number[]{
  return a.concat([[e]]);
}
