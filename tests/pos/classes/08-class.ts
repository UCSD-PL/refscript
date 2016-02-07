
export function ext(x: number) { }

class Foo<M extends ReadOnly> {

  // Static method
  static s(x: number) {}

  // Method
  m(x: number, y: number) {}

  // Function field
  f = ext;

  constructor() {}
}

let a: Foo<ReadOnly> = new Foo();
Foo.s(1);
a.m(1,2);
a.f(1);
