class Foo{}

/*@ bar :: () => {void | true} */
function bar() {}

/*@ foo :: /\ (f: Foo<Mutable>) => {()=>void|true}
           /\ (f: ()=>void) => {()=>void|true} */
function foo(f:any) { 
  return (typeof f === "function") ? f : bar
}

