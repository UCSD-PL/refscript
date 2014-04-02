class A {

	/*@ constructor :: (x: number) => void */
	constructor(x: number) { }

}

class B extends A {

}

class C extends B {

	/*@ constructor :: () => void */
	constructor() {
    super(1); 
  }

}
