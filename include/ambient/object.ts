


/*************************************************************************
 *
 *    AMBIENT DEFINITIONS 
 *
 *    Taken from here: 
 *
 *    http://typescript.codeplex.com/sourcecontrol/latest#typings/core.d.ts
 *
 *************************************************************************/


/**
 *  OBJECT 
 *
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L80
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L115
 *
 */

interface Object {
    // TODO
    /** The initial value of Object.prototype.constructor is the standard built-in Object constructor. */
    // constructor: Function;

    /** Returns a string representation of an object. */
    toString(): string;

    /** Returns a date converted to a string using the current locale. */
    toLocaleString(): string;

    /** Returns the primitive value of the specified object. */
    valueOf(): Object;

    /**
      * Determines whether an object has a property with the specified name. 
      * @param v A property name.
      */
    // TODO: this can be simplified if we had as invariant that hasDirectProperty => hasProperty
    /*@ hasOwnProperty : forall A . (this: A, p: string) 
                       : { boolean | Prop(v) <=> hasDirectProperty(p, this) && hasProperty(p, this)}
     */
    hasOwnProperty(p: string): boolean;

    /**
      * Determines whether an object exists in another object's prototype chain. 
      * @param v Another object whose prototype chain is to be checked.
      */
    isPrototypeOf<A>(v: A): boolean;

    /** 
      * Determines whether a specified property is enumerable.
      * @param v A property name.
      */
    propertyIsEnumerable(v: string): boolean;
}



declare var Object: {
    new <A>(value: A): Object;						// new (value?: any): Object;
    (): any;
    <A>(value: A): any;								// (value: any): any;

    prototype: Object;

    getPrototypeOf<A>(o: A): any;					// getPrototypeOf(o: any): any;

    // getOwnPropertyDescriptor(o: any, p: string): PropertyDescriptor;

    getOwnPropertyNames<A>(o: A): string[];			// getOwnPropertyNames(o: any): string[];

    // create(o: any, properties?: PropertyDescriptorMap): any;

    // defineProperty(o: any, p: string, attributes: PropertyDescriptor): any;

    // defineProperties(o: any, properties: PropertyDescriptorMap): any;

    // seal(o: any): any;

    // freeze(o: any): any;

    // preventExtensions(o: any): any;

    // isSealed(o: any): boolean;

    // isFrozen(o: any): boolean;

    // isExtensible(o: any): boolean;

    keys<A>(o: A): string[];						// keys(o: any): string[];
}


