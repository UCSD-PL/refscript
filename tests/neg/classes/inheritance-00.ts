/*@ alias nat = {v: number | v >= 0 } */


interface IPoint {
  /*@ x : number */
  x: number;
  /*@ y : number */
  y: number;
}

interface INatPoint {
  /*@ x : nat */
  x: number;
  /*@ y : nat */
  y: number;
}

interface IColorPoint extends IPoint {
  c: string;
}

class A {
  /*@ a : nat */
  a: number;
	/*@ foo: (): void */
	private foo(): void {  }
	/*@ bar: (x: IPoint): void */
	bar(x: IColorPoint): void {  }
	/*@ baz: (x: IPoint): void */
	baz(x: INatPoint): void {  }

  constructor() {  } 
}

class B extends A { 
  constructor() { super(); } 
}

class C extends B {
  /*@ a : nat */
  a: number; 
	/*@ baz: (x: INatPoint): void */
	baz(x: INatPoint): void {  }
  constructor() { super(); } 
}

