
class Animal<M extends ReadOnly> {
  /*@ (Immutable) kind : { v: string | [ ((v = "") || (v = "horse") || (v = "snake") || (v = "tiger"));
                                         (v = "horse") => extends_class(this, "Horse");
                                         (v = "snake") => extends_class(this, "Snake");
                                         (v = "tiger") => extends_class(this, "Tiger")] } */
  public kind = "";
  constructor() {}
}

class Horse<M extends ReadOnly> extends Animal<M> { 
  public gallop() {} 
  constructor() { 
    super(); 
    this.kind = "no horse";
  }
}

class Snake<M extends ReadOnly> extends Animal<M> { 
  public sneak() {}
  constructor() { 
    super(); 
    this.kind = "no snake";
  }
}

/*@ move :: (a: Animal<Mutable>) => { void | 0 < 1} */
function move(a: Animal<Mutable>) {

  if (a.kind === "snake") {
    let s = <Snake<Mutable>>a;
    s.sneak();
  }

}

move(new Horse()); 
