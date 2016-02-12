
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
    this.kind = "horse";
  }
}

class Snake<M extends ReadOnly> extends Animal<M> { 
  public sneak() {}
  constructor() { 
    super(); 
    this.kind = "snake";
  }
}

class Tiger<M extends ReadOnly> extends Animal<M> { 
  public run() {}
  constructor() { 
    super(); 
    this.kind = "tiger";
  }
}

/*@ move :: (a: Animal<Immutable>) => { void | 0 < 1} */
function move(a: Animal<Immutable>) {
  let k = a.kind;
  if (a.kind === "horse") {
    let h = <Horse<Immutable>>a;
    h.gallop();
  }
  else if (a.kind === "tiger") {
    let s = <Snake<Immutable>>a;
    s.sneak();
  }
}
