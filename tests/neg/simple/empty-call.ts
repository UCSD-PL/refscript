

declare function foo(b: boolean): void;

/*@  bar :: () => { void | true } */
function bar() {

  foo(true);

  assert(false);


}
