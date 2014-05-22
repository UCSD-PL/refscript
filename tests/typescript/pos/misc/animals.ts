

class Animal {
  
  /*@ name :: string */
  public name;

  /*@ constructor :: (string) => void */
  constructor(name) { 
    this.name = name;
  }

  /*@ move :: (number) => void */
  move(meters) {
    alert(this.name + " moved " + meters + "m.");
  }

}


class Snake extends Animal {
  /*@ move :: () => void */  
  move() {
    alert("Slithering...");
    super.move(5);
  }

}

class Horse extends Animal {
  /*@ move :: () => void */
  move() {
    alert("Galloping...");
    super.move(45);
  }

}

var sam = new Snake("Sammy the Python")
var tom: Animal = new Horse("Tommy the Palomino")

sam.move();
//XXX: Weirdness in parameters
tom.move(34)
