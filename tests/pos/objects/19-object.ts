
// We can reference an object in the context of
// an object literal member after making it mutable.

/*@ readonly uniqueInnerObj :: { @Assignable n : number } */
let uniqueInnerObj = { n: 6 };

let uniqueObj = { a: 5, b: "String", c: uniqueInnerObj };
