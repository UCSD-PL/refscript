
/*@ option --extrainvs */

// Custom objects
var mycar = {make: "Honda", model: "Accord", year: 1998};
assert("make?" in mycar);  // returns false
assert("model_" in mycar); // returns false
