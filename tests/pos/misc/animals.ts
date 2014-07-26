

class Animal {
  
  /*@ name : string */
  public name;

  /*@ new (string) => void */
  constructor(name) { 
    //this.name = name;
  }

  /*@ move : (number): void */
  move(meters) {
    alert(this.name + " moved " + meters + "m.");
  }

}

/*@ class Snake extends #Animal */
class Snake extends Animal {
  /*@ move : (): void */
  move() {
    alert("Slithering...");
    super.move(5);
  }

}


/*@ class Snake extends #Animal */
class Horse extends Animal {
  /*@ move : (meters: number): void */
  move(meters) {
    alert("Galloping...");
    super.move(45);
  }

}

var sam = new Snake("Sammy the Python")

/*@ tom :: #Animal */
var tom: Animal = new Horse("Tommy the Palomino")

sam.move();
//XXX: Weirdness in parameters
tom.move(34)
