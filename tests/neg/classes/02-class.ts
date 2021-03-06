
/*@ alias nat    = {v: number | v >= 0 }  */

interface IPoint {
  x: number;
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
	private foo(): void {  }
	bar(x: IPoint): void {  }
	baz(x: INatPoint): void {  }
  constructor() {}
}


class B extends A {
	bar(x: IColorPoint): void {  }
	baz(x: IPoint): void {  }
  constructor() { super(); }
}
