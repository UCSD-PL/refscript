class Greeter {
  
  a /*@ string */;

  private b /*@ number */;
  
  private c /*@ number */ = 1;
  
  public  d /*@ number */;
  
  private static e /*@ { } */ = { };

  
  /*@ (message: string) => void */
  constructor(message) {
    //this.greeting = message;
  }
  /*@ () => void */
  private greet1() {
    //return "Hello, " + this.greeting;
  }

  /*@ () => void */
  greet2() {
    //return "Hello, " + this.greeting;
  }

  /*@ () => void */
  public greet3() {
    //return "Hello, " + this.greeting;
  }
}
