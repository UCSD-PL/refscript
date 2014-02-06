class Greeter {
  
  a /*@ string */;

  private b /*@ number */;
  
  private c /*@ string */ = 1;
  
  public  d /*@ number */;
  
  private static e /*@ { } */ = { };

  /*@ (message: string) => void */
  constructor(message) { }
  /*@ () => void */
  private greet1() { }

  /*@ () => void */
  greet2() {  }

  /*@ () => void */
  public greet3() { }
}
