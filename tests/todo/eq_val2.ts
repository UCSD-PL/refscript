


class A {
  
  public x;

  public y;

  /*@ new (a: number) => void */
  constructor(a: number) {
    this.x = a;
    this.y = a;
  }

}

/*@ foo :: () => {void | true} */
function foo(){
  var r = new A(29);
  var p = r.x;
  var q = r.y;
  assert (p === q); 
}

