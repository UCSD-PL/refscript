
/*@ grow :: forall T . (array: #Array[#Mutable,T], length: number, defaultValue: T) => { void | true } */

function grow<T>(array: T[], length: number, defaultValue: T): void {
	var count = length - array.length;
	for (var i = 0; i < count; i++) {
		array.push(defaultValue);
	}
}

