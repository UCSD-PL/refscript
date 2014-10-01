class Greeter {
	/*@ a: string */
	               a = "" ;

	/*@ b : number */
	private        b = 2;

	/*@ c : number */ 
	private        c = 3;
	
	/*@ d : number */
	public         d = 4;
	
	/*@ static e : { } */
	private static e = { };
	
	/*@ new (message: string) => void */
	constructor(message) { }

	/*@ greet1 : (): void */
	private greet1() { }

	/*@ greet2 : (): void */
	greet2() { }

	/*@ greet3 : (): void */
	public greet3() { }
}
