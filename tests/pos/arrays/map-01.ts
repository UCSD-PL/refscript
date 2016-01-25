
export function bar(arr: IArray<number>, f: (a: number, b: number) => string): IArray<string> {
  return arr.map(f);
}
