class Blah<M extends ReadOnly> {
    constructor() { }

    public state = 3;

    public foo(): void {
        /*@ val :: number + null */
        let val = null;
        if (this.state === 42)
            val = 7;
    }
}
