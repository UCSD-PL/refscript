export function foo(arr: IArray<string>): boolean {
  if (arr.length > 0) {
    if (arr[0] === "blah") {
      return true;
    }
  }
  return false;
}

export function bar(arr: IArray<string>): boolean {
  if ((arr.length > 0) && (arr[0] === "blah")) {
    return true;
  }
  return false;
}
