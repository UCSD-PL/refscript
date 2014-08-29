class A {

	/*@ new (x: number) => void */
	constructor(x: number) { }

}

class B extends A {

}

class C extends B {

	/*@ new () => void */
	constructor() {
    super(1); 
  }

}
