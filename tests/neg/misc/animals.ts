class Animal<M extends ReadOnly> {
  public kind = "";
  constructor() { }
}
class Horse<M extends ReadOnly> extends Animal<M> { 
  public kind = "horse";
  public gallop() {} 
  constructor() { super(); }
}
class Snake<M extends ReadOnly> extends Animal<M> { 
  public kind = "snake";
  public sneak() {} 
  constructor() { super(); }
}

/*@ move :: (a: Animal<Immutable>) => { void | 0 < 1} */
function move(a: Animal<Immutable>) {

  if (a.kind === "snake") {
    let h = <Horse<Immutable>>a;
    h.gallop();
  }
  else if (a.kind === "horse") {
    let s = <Snake<Immutable>>a;
    s.sneak();
  }

}
