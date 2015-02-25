class Greeter {
    /*@ a : string */
    a = "";

    /*@ b : number */
    private b = 2;

    /*@ c : number */ 
    private c = 3;
    
    /*@ d : number */
    public  d = 4;
    
    /*@ e : { } */
    private static e = { };
    
    constructor(message: string ) { }

    /*@ greet1 : (): void */
    private greet1() { }

    /*@ greet2 : (): void */
    greet2() { }

    /*@ greet3 : (): void */
    public greet3() { }
}
