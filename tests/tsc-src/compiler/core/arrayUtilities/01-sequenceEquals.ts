
// TODO: The version with the ...? does not work yet.
/*  sequenceEquals :: forall T . (array1: [T]?, array2: [T]?, equals: (T,T) => boolean) => { boolean | true } */

/*@ sequenceEquals :: forall T . ( array1: #Array[#Immutable,T]
                                 , array2: #Array[#Immutable,T]
                                 , equals: (T,T) => boolean ) => { v: boolean | true } */

function sequenceEquals<T>(array1: T[], array2: T[], equals: (v1: T, v2: T) => boolean) {
    if (array1 === array2) {
        return true;
    }

    if (array1 === null || array2 === null) {
        return false;
    }
    
    if (array1.length !== array2.length) {
        return false;
    }

    assert(array1.length === array2.length);

    for (var i = 0, n = array1.length; i < n; i++) {
        if (!equals(array1[i], array2[i])) {
            return false;
        }
    }

    return true;
}
