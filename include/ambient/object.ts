/// <reference path="../rsc/mutability.ts"/>

interface Object {}

/*

    The following are taken from here:

    https://github.com/Microsoft/TypeScript/blob/master/lib/lib.d.ts#L82

    and have been adjusted with mutability modifiers.

 */

interface ObjectConstructor<M extends ReadOnly> {
    new (value?: any): Object;
    (): any;
    /*@ (value: string): string */
    (value: any): any;

    /** A reference to the prototype for a class of objects. */
    prototype: Object;

    /**
      * Returns the prototype of an object.
      * @param o The object that references the prototype.
      */
     
    /*@ getPrototypeOf(o: string): { string | v = "" }  */
    getPrototypeOf(o: any): any;

    /**
      * Gets the own property descriptor of the specified object.
      * An own property descriptor is one that is defined directly on the object and is not inherited from the object's prototype.
      * @param o Object that contains the property.
      * @param p Name of the property.
    */
    getOwnPropertyDescriptor(o: any, p: string): PropertyDescriptor<M>;

    /**
      * Returns the names of the own properties of an object. The own properties of an object are those that are defined directly
      * on that object, and are not inherited from the object's prototype. The properties of an object include both fields (objects) and functions.
      * @param o Object that contains the own properties.
      */
    getOwnPropertyNames(o: any): string[];

    /**
      * Creates an object that has the specified prototype, and that optionally contains specified properties.
      * @param o Object to use as a prototype. May be null
      * @param properties JavaScript object that contains one or more property descriptors.
      */
    create(o: any, properties?: PropertyDescriptorMap<M>): any;

    /**
      * Adds a property to an object, or modifies attributes of an existing property.
      * @param o Object on which to add or modify the property. This can be a native JavaScript object (that is, a user-defined object or a built in object) or a DOM object.
      * @param p The property name.
      * @param attributes Descriptor for the property. It can be for a data property or an accessor property.
      */
    defineProperty(o: any, p: string, attributes: PropertyDescriptor<M>): any;

    /**
      * Adds one or more properties to an object, and/or modifies attributes of existing properties.
      * @param o Object on which to add or modify the properties. This can be a native JavaScript object or a DOM object.
      * @param properties JavaScript object that contains one or more descriptor objects. Each descriptor object describes a data property or an accessor property.
      */
    defineProperties(o: any, properties: PropertyDescriptorMap<M>): any;

    /**
      * Prevents the modification of attributes of existing properties, and prevents the addition of new properties.
      * @param o Object on which to lock the attributes.
      */
    seal<T>(o: T): T;

    /**
      * Prevents the modification of existing property attributes and values, and prevents the addition of new properties.
      * @param o Object on which to lock the attributes.
      */
    freeze<T>(o: T): T;

    /**
      * Prevents the addition of new properties to an object.
      * @param o Object to make non-extensible.
      */
    preventExtensions<T>(o: T): T;

    /**
      * Returns true if existing property attributes cannot be modified in an object and new properties cannot be added to the object.
      * @param o Object to test.
      */
    isSealed(o: any): boolean;

    /**
      * Returns true if existing property attributes and values cannot be modified in an object, and new properties cannot be added to the object.
      * @param o Object to test.
      */
    isFrozen(o: any): boolean;

    /**
      * Returns a value that indicates whether new properties can be added to an object.
      * @param o Object to test.
      */
    isExtensible(o: any): boolean;

    /**
      * Returns the names of the enumerable properties and methods of an object.
      * @param o Object that contains the properties and methods. This can be an object that you created or an existing Document Object Model (DOM) object.
      */
    keys(o: any): string[];
}

/**
  * Provides functionality common to all JavaScript objects.
  */
declare var Object: ObjectConstructor<ReadOnly>;


interface PropertyDescriptor<M> {
    configurable?: boolean;
    enumerable?: boolean;
    value?: any;
    writable?: boolean;
    get? (): any;
    set? (v: any): void;
}

interface PropertyDescriptorMap<M extends ReadOnly> {
    [s: string]: PropertyDescriptor<ReadOnly>;
}
