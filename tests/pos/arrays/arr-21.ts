//adapted from tsc

/*@ filter :: (x: boolean) => IArray<string> + number */
export function filter(x):any {
  if (x) return 42;
  return new Array<string>(3);
}
