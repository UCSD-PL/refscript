

interface IA<T> {
  x: T;
}


interface IB<T> {
  y: T;
}

interface IC<S> extends IA<number>, IB<S> {
	z: IB<S>;
}


interface ID<T> {
  [ y: string]: T;
  [ y: number ]: T;
}