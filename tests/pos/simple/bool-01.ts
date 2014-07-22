
var emp = [];

//Needed to infer mutability
emp.push(1);
emp.pop();

assert(!(emp === null));

