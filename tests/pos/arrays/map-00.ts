
export function foo(arr: IArray<number>, f: (x: number) => string): IArray<string> {
  return arr.map(f);
}
