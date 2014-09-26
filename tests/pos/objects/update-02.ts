/*@ foo ::  ([#Mutable]{ x: { v: number | v = 10 } }) => { v: number | v = 20 } */ 
function foo(o) :number{ 
  o.x = 10;
  return o.x + 10
}
