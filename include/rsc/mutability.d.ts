
interface ReadOnly {
    _readOnnlyBrand: any;
}

interface AssignsFields extends ReadOnly {
    _assignsFieldsBrand: any;
}

interface Mutable extends AssignsFields {
    _mutableBrand: any;
}

interface Immutable extends ReadOnly {
    _immutableBrand: any;
}

interface Unique extends Immutable, Mutable {
    _uniqueBrand: any;
}
