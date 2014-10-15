class Animal {
  public kind = "";
}
class Horse extends Animal { 
  public kind = "horse";
  public gallop() {} 
}
class Snake extends Animal { 
  public kind = "snake";
  public sneak() {} }

/*@ move :: (a: Animal) => { void | true } */
function move(a: Animal) {

  if (a.kind === "snake") {
    var h = <Horse>a;
    h.gallop();
  }
  else if (a.kind === "horse") {
    var s = <Snake>a;
    s.sneak();
  }

}

