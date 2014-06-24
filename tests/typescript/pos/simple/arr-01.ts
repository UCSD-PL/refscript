
// var a = [];
// a.push(1);
// var b = Array.isArray(a);
// a.concat([2]);

/*@ ext :: (x: number) => void */
function ext(x: number) { }

class Foo {

  // Static method
  /*@ s :: (x: number) => void */
  static s(x: number) {}

  // Method
  /*@ m :: (x: number, y: number): void */
  m(x: number, y: number) {}

  // Function field
  /*@ f :: (x: number) => void */
  f = ext;
}


var a = new Foo();

Foo.s(1);

a.m(1,2);

a.f(1);
