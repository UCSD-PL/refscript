//adapted from richards-typed-octane.ts

module RichardsTYPEDVERSION {
    /*@ readonly STATE_SUSPENDED :: bitvector32 */
    let STATE_SUSPENDED = 0x00000002;

    class Task<M extends ReadOnly> {
        constructor() {}
    }

    class HandlerTask<M extends ReadOnly> extends Task<M> {
        constructor() {
            super();
        }
    }
    // let h = new HandlerTask()
}
