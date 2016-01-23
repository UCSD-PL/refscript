
/*@ type pos = { number | v > 0 } */
type pos = number;

/*@ foo :: (x: boolean + pos) => void */
declare function foo(x: number | boolean): void;

foo(1);
