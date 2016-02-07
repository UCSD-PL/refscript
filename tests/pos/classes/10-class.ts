declare class Foo{}

/*@ bar :: () => {void | 0 < 1} */
function bar() {}

/*@ foo :: /\ (f: Foo<Mutable>) => {()=>void|0 < 1}
           /\ (f: ()=>void) => {()=>void|0 < 1} */
function foo(f:any) { 
  return (typeof f === "function") ? f : bar
}

