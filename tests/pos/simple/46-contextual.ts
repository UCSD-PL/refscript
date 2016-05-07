
interface IPoint<M extends ReadOnly> { x: number; y: number }

export declare function bar(p: IPoint<ReadOnly>): void;

bar({ x: 1, y: 2 });
