
interface IPoint {
  /*@ x :: { number | v >= 0 } */
  x: number;
  /*@ y :: { number | v >= 0 } */
  y: number;
}

interface INatPoint extends IPoint {
  x: number;
  y: number;
}


interface IColorPoint extends IPoint {
  c: string;
}


