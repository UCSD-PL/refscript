
/*@ foo :: forall A . ([ [ number ] ], number ) => { [ [ number ] ] | true } */

function foo(a, e) {
  return a.concat([[e]]);
}
