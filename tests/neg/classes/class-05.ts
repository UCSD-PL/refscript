interface IPoint {
  /*@ x : { number | 0 < 1 } */
  x: number;
  /*@ y : { number | 0 < 1 } */
  y: number;
}

interface INatPoint {
  /*@ x : { number | v >= 0 } */
  x: number;
  /*@ y : { number | v >= 0 } */
  y: number;
}


interface IColorPoint extends IPoint {
  c: string;
}


class A {

  /*@ a : { number | v > 0 } */
  a: number; 

	/*@ foo : (): void */
	private foo(): void {  }

	/*@ bar: (x: #IPoint): void */
	bar(x: IColorPoint): void {  }

  constructor() {}
}


class B extends A {

  /*@ a : number */
  a: number; 

	/*@ bar : (x: #IColorPoint): void */
	bar(x: IPoint): void {  }

  constructor() { super(); }

}
