
class Animal {
 
  /*@ kind : [Immutable] { v: string | [((v = "") || (v = "horse") || (v = "snake") || (v = "tiger"));
                                        (v = "horse") => extends_class(this, "Horse");
                                        (v = "snake") => extends_class(this, "Snake")] } */
  public kind: string = "";
  constructor() {}
}

class Horse extends Animal { 
  public gallop() {} 
  constructor() { 
    super(); 
    this.kind = "horse";
  }
}
class Snake extends Animal { 
  public sneak() {} 
  constructor() {
    super(); 
    this.kind = "snake";
  }
}

/*@ move :: (a: Animal<Immutable>) => { void | true } */
function move(a: Animal) {
  if (a.kind === "horse") {
    let h = <Horse>a;
    h.gallop();
  }
  else if (a.kind === "snake") {
    let s = <Snake>a;
    s.sneak();
  }
}
