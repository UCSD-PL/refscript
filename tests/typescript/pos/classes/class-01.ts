class Greeter {
	/*@ a: string */
	a;

	/*@ b : number */
	private b;
	
	/*@ c : number */ 
	private c = 1;
	
	/*@ d : number */
	public  d;
	
	/*@ static eee : { } */
	private static eee = { };
	
	/*@ new (message: string) => void */
	constructor(message) { }

	/*@ greet1 : (): void */
	private greet1() { }

	/*@ greet2 : (): void */
	greet2() { }

	/*@ greet3 : (): void */
	public greet3() { }
}
