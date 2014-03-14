
interface A<T> {
  f: T;

  /*@ g :: { number | v > 0 } */
  g: number;

}

/*@ interface C<T> extends A<{number | v <0 }> */
interface C<T> extends A<number> {
  minChar: number;
  limChar: number;
  trailingTriviaWidth: number;
}


class B {

  public a: number;

}
