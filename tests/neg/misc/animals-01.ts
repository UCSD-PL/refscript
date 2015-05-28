

class Animal {
  /*@ kind : [Immutable] { v: string | [ ((v = "") || (v = "horse") || (v = "snake") || (v = "tiger"));
                                         (v = "horse") => extends_class(this, "Horse");
                                         (v = "snake") => extends_class(this, "Snake");
                                         (v = "tiger") => extends_class(this, "Tiger")] } */
  public kind = "";
  constructor() {}
}

class Horse extends Animal { 
  public gallop() {} 
  constructor() { 
    super(); 
    this.kind = "no horse";
  }
}

class Snake extends Animal { 
  public sneak() {}
  constructor() { 
    super(); 
    this.kind = "no snake";
  }
}

/*@ move :: (a: Animal<Mutable>) => { void | 0 < 1} */
function move(a: Animal) {

  if (a.kind === "snake") {
    var s = <Snake>a;
    s.sneak();
  }

}

move(new Horse()); 


