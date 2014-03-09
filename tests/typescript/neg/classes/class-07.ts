class A {

}

// `A` here is an ubnounded type variable - it's not the class type #A 
// So this should fail ...

/*@ a :: A */
var a : A = new A();

