
/*@ ext :: (x: number) => void */
function ext(x: number) { }

class Foo {

  // Static method
  static s(x: number) {}

  // Method
  m(x: number, y: number) {}

  // Function field
  f = ext;

  constructor() {}

}


var a = new Foo();

Foo.s(1);

a.m(1,2);

a.f(1);
