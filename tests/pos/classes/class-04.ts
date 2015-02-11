class A {
	constructor(x: number) { }
}

class B extends A {
  constructor () { super(1); }
}

class C extends B {
	constructor() { super(); }
}
