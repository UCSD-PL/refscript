
/*@ alias nat    = {v: number | v >= 0 }  */

interface IPoint {
  x: number;
  y: number;
}

interface INatPoint {
  /*@ x :: { number | v >= 0 } */
  x: number;
  /*@ y :: { number | v >= 0 } */
  y: number;
}


interface IColorPoint extends IPoint {
  c: string;
}


class A {

  /*@ a :: { number | v > 0 } */
  a: number; 

	/*@ foo :: () => void */
	private foo(): void {  }

	/*@ bar :: (x: #IColorPoint) => void */
	bar(x: IColorPoint): void {  }

	/*@ baz :: (x: #INatPoint) => void */
	baz(x: INatPoint): void {  }
}


class B extends A {

  /*@ a :: number */
  a: number; 

	/*@ bar :: (x: #IPoint) => void */
	bar(x: IPoint): void {  }

	/*@ baz :: (x: #IPoint) => void */
	baz(x: IPoint): void {  }

}
