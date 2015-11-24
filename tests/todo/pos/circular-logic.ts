foo();
/*@ foo :: () => {void | false} */
function foo() { } // this should fail, but the call on line 1 lets false into the environment so it passes?
