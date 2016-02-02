
// Cannot reference unique object in the context of
// an object literal member


let uniqueInnerObj = { n: 6 };

let uniqueObj = { a: 5, b: "String", c: uniqueInnerObj };
