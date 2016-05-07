
declare function fromIArray(x: IArray<number>, y: IArray<number>): void;

let a: IArray<number> = new Array<number>(10);

fromIArray(a, a);
