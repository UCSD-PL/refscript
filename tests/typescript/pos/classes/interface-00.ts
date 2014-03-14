
interface A<T> {
  f: T;
}


interface C<T> extends A<number> {
  minChar: number;
  limChar: number;
  trailingTriviaWidth: number;
  g: T;
}


class B {

  public a: number;

}
