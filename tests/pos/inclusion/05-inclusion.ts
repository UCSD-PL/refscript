/*@ option --extrainvs */

// Predefined objects
// assert("PI" in Math);         // returns true

// Custom objects

let mycar = {make: "Honda", model: "Accord", year: 1998};
assert("make" in mycar);  // returns true
assert("model" in mycar); // returns true
