class Greeter {
	/*@ a :: string */
	a;

	/*@ b :: number */
	private b;
	
	/*@ c :: string */ 
	private c = 1;
	
	/*@ d :: number */
	public  d ;
	
	/*@ e :: { } */
	private static e = { };

	/*@ constructor :: (message: { string | v = "a" } ) => void */
	constructor(message) { }

	/*@ greet1 :: () => void */
	private greet1():void { 
    this.c = 1;
  }

	/*@ greet2 :: () => void */
	greet2() : void {  }

	/*@ greet3 :: () => void */
	public greet3() :void { }
}
