// ///<reference path='references.ts' />

///<reference path='../../../../include/prelude.ts' />

// IMPORTS BEGIN
interface IIndexable<T> {
	[s: string]: T;
}
// IMPORTS END



//module TypeScript {
    /*export */class ArrayUtilities1 {

        //XXX: Not supported yet
        ///*@ isArray :: forall A . (value: A) => { boolean | true } */
        //public static isArray(value: any): boolean {
        //    return Object.prototype.toString.apply(value, []) === '[object Array]';
        //}

        /*@ sequenceEquals :: forall T . ( array1: #Array[#Immutable,T]
                                 , array2: #Array[#Immutable,T]
                                 , equals: (T,T) => boolean ) => { v: boolean | true } */
        public static sequenceEquals<T>(array1: T[], array2: T[], equals: (v1: T, v2: T) => boolean) {
            if (array1 === array2) {
                return true;
            }

            if (array1 === null || array2 === null) {
                return false;
            }

            if (array1.length !== array2.length) {
                return false;
            }

            for (var i = 0, n = array1.length; i < n; i++) {
                if (!equals(array1[i], array2[i])) {
                    return false;
                }
            }

            return true;
        }

        /*@ contains :: forall T . (a: #Array[#Immutable,T], value: T) => { boolean | true } */
        public static contains<T>(array: T[], value: T): boolean {
            for (var i = 0; i < array.length; i++) {
                if (array[i] === value) {
                    return true;
                }
            }

            return false;
        }

        /*@ groupBy :: forall T . (array: #Array[#Immutable,T], f: (T)=>string) 
                    => { #IIndexable[ #Array[#Mutable,T] ] | true } */
        public static groupBy<T>(array: T[], f: (v: T) => string): any {

          /*@ result_ :: #IIndexable[ #Array[#Mutable,T] ] */
          var result_: IIndexable<T[]> = {};

          for (var i = 0, n = array.length; i < n; i++) {
                var v: any = array[i];
                var k = f(v);

                var list: T[] = result_[k] || [];
                list.push(v);
                result_[k] = list;
            }

            return result_;
        }


        // XXX: Not supported
        //// Gets unique element array
        ///*@ distinct :: forall T . (a: [T], f: (T)=>string) => IIndexable<[T]> */
        //public static distinct<T>(array: T[], equalsFn?: (a: T, b: T) => boolean): T[] {
        //    /*@ result: mutable [T] */
        //    var result: T[] = [];

        //    // TODO: use map when available
        //    for (var i = 0, n = array.length; i < n; i++) {
        //        var current = array[i];
        //        for (var j = 0; j < result.length; j++) {
        //            if (equalsFn(result[j], current)) {
        //                break;
        //            }
        //        }

        //        if (j === result.length) {
        //            result.push(current);
        //        }
        //    }

        //    return result;
        //}

        /*@ min :: forall T . (array: { #Array[#Immutable,T] | (len v) > 0 } , f: (T)=>number) => number */
        public static min<T>(array: T[], f: (v: T) => number): number {
            // Debug.assert(array.length > 0);
            var min_ = f(array[0]);

            for (var i = 1; i < array.length; i++) {
                var next = f(array[i]);
                if (next < min_) {
                    min_ = next;
                }
            }

            return min_;
        }

        /*@ max :: forall T . (array: { #Array[#Immutable,T] | (len v) > 0 }, f: (T)=>number) => number */
        public static max<T>(array: T[], f: (v: T) => number): number {
            // Debug.assert(array.length > 0);
            var max_ = f(array[0]);

            for (var i = 1; i < array.length; i++) {
                var next = f(array[i]);
                if (next > max_) {
                    max_ = next;
                }
            }

            return max_;
        }

        /*@ last :: forall T . (array: #Array[#Immutable,T]) => { T | true } */
        public static last<T>(array: T[]): T {
            if (array.length === 0) {
                throw Errors.argumentOutOfRange('array');
            }

            return array[array.length - 1];
        }

        /*@ lastOrDefault :: forall T . (array: #Array[#Immutable,T], (v:T, index: number) => boolean) => { T | true }  */
        public static lastOrDefault<T>(array: T[], predicate: (v: T, index: number) => boolean): T {
            for (var i = array.length - 1; i >= 0; i--) {
                var v = array[i];
                if (predicate(v, i)) {
                    return v;
                }
            }

            //PV
            throw new Error("Cannot unify null with T.")
            return null;
        }

        /*@ firstOrDefault :: forall T . (array: #Array[#Immutable,T], f : (v: T, index: number) => boolean) => { T | true } */
        public static firstOrDefault<T>(array: T[], f: (v: T, index: number) => boolean): T {
            for (var i = 0, n = array.length; i < n; i++) {
                var value = array[i];
                if (f(value, i)) {
                    return value;
                }
            }

            //PV
            throw new Error("Cannot unify null with T.")
            return null;
        }

        /*@ first :: forall T . (array: #Array[#Immutable,T], f: (v: T, index: number) => boolean) => { T | true } */
        public static first<T>(array: T[], f/*?*/: (v: T, index: number) => boolean): T {
            for (var i = 0, n = array.length; i < n; i++) {
                var value = array[i];
                if (!f || f(value, i)) {
                    return value;
                }
            }

            throw Errors.invalidOperation("");
        }

        /*@ sum :: forall T . (array: #Array[#Immutable,T], f: (T) => number) => { number | true } */
        public static sum<T>(array: T[], f: (v: T) => number): number {
            var result = 0;

            for (var i = 0, n = array.length; i < n; i++) {
		            result = result + f(array[i]);
                //result += f(array[i]);
            }

            return result;
        }

        /*@ select :: forall T S . (values: #Array[#Immutable,T], f: (T) => S) => { #Array[#Immutable,S] | true } */
        public static select<T, S>(values: T[], f: (v: T) => S): S[]{
            var result: S[] = new Array<S>(values.length);

            for (var i = 0; i < values.length; i++) {
                result[i] = f(values[i]);
            }

            return result;
        }

        /*@ where :: forall T . (values: #Array[#Immutable,T], f: (T) => boolean) 
                  => { #Array[#ReadOnly,T] | true } */
        public static where<T>(values: T[], f: (v: T) => boolean): T[] {
            var result = new Array<T>(0); //PV added 0

            for (var i = 0; i < values.length; i++) {
                if (f(values[i])) {
                    result.push(values[i]);
                }
            }

            return result;
        }

        /*@ any :: forall T . (array: #Array[#Immutable,T], f: (T) => boolean) => { boolean | true } */
        public static any<T>(array: T[], f: (v: T) => boolean): boolean {
            for (var i = 0, n = array.length; i < n; i++) {
                if (f(array[i])) {
                    return true;
                }
            }

            return false;
        }

        /*@ all :: forall T . (array: #Array[#Immutable,T], f: (T) => boolean) => { boolean | true } */
        public static all<T>(array: T[], f: (v: T) => boolean): boolean {
            for (var i = 0, n = array.length; i < n; i++) {
                if (!f(array[i])) {
                    return false;
                }
            }

            return true;
        }

        /*@ binarySearch :: (array: #Array[#Immutable,number], value: number) 
                         => { v: number | (0 <= v && (v < (len array)))} */
        public static binarySearch(array: number[], value: number): number {
            var low = 0;
            var high = array.length - 1;

            while (low <= high) {
		            //var miGddle = low + ((high - low) >> 1);
		            var middle = low + ((high - low) / 2);
                var midValue = array[middle];

                if (midValue === value) {
                    return middle;
                }
                else if (midValue > value) {
                    high = middle - 1;
                }
                else {
                    low = middle + 1;
                }
            }

            //PV 
            throw new Error("Number not found");
          	//return ~low;
        }

        /*@ createArray :: forall T . (length: number, defaultValue: T) => { #Array[#Immutable,T] | true } */
        public static createArray<T>(length: number, defaultValue: any): T[]{
            var result = new Array<T>(length);
            for (var i = 0; i < length; i++) {
                result[i] = defaultValue;
            }

            return result;
        }

        /*@ grow :: forall T . (array: #Array[#Mutable,T], length: number, defaultValue: T) => { void | true } */
        public static grow<T>(array: T[], length: number, defaultValue: T): void {
            var count = length - array.length;
            for (var i = 0; i < count; i++) {
                array.push(defaultValue);
            }
        }

        /*@ copy :: forall T . 
              (sourceArray: #Array[#Immutable,T] ,
              sourceIndex: { number | 0 <= v } , 
              destinationArray: #Array[#Immutable,T], 
              destinationIndex: { number | 0 <= v } , 
              length: { v:number | ((sourceIndex      + v <= (len sourceArray)) 
                                  && (destinationIndex + v <= (len destinationArray)))}) 
            => void */
        public static copy<T>(sourceArray: T[], sourceIndex: number, destinationArray: T[], destinationIndex: number, length: number): void {
            for (var i = 0; i < length; i++) {
                destinationArray[destinationIndex + i] = sourceArray[sourceIndex + i];
            }
        }

        /*@ indexOf :: forall T . (array: #Array[#Immutable,T], predicate: (T) => boolean) 
                    => { number | (0 <= v && v < (len array)) } */
        public static indexOf<T>(array: T[], predicate: (v: T) => boolean): number {
            for (var i = 0, n = array.length; i < n; i++) {
                if (predicate(array[i])) {
                    return i;
                }
            }
            throw new Error("Not found");
          	//return -1;
        }
    }
//}
