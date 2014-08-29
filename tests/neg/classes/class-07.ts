class A {

}

// `A` here is an ubnounded type variable - it's not the class type #A 
// So this should fail ...

/*@ a :: {v : A | true} */
var a : A = new A();

