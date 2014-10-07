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

function move(a: Animal) {

  if (a.kind === "horse") {
    var h = <Horse>a;
    h.gallop();
  }
  else if (a.kind === "snake") {
    var s = <Snake>a;
    s.sneak();
  }

}

