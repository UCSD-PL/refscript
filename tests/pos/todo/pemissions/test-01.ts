

/*@ g :: (a: { int | v = b.x }, b: { @IM x: int; @MU y: int }) => void */
declare function g(a: number, b: { x: number; y: number }): void;

g(1, { x: 1, y: 1 });
