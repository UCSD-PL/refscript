// Copyright 2006-2008 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


// This is a JavaScript implementation of the Richards
// benchmark from:
//
//    http://www.cl.cam.ac.uk/~mr10/Bench.html
//
// The benchmark was originally implemented in BCPL by
// Martin Richards.

/*@ alias nat = {number | 0 <= v} */
/*@ alias natLT[num] = {nat | v < num} */

/*@ alias MSelf = Self<Mutable> */
/*@ alias MTask = Task<Mutable> */
/*@ alias MScheduler = Scheduler<Mutable> */
/*@ alias MPacket = Packet<Mutable> */
/*@ alias MTaskControlBlock = TaskControlBlock<Mutable> */

module RichardsTYPEDVERSION {
    /*@ readonly COUNT :: # */
    var COUNT = 1000;

    /**
     * These two constants specify how many times a packet is queued and
     * how many times a task is put on hold in a correct run of richards.
     * They don't have any meaning a such but are characteristic of a
     * correct run so if the actual queue or hold count is different from
     * the expected there must be a bug in the implementation.
     **/
    /*@ readonly EXPECTED_QUEUE_COUNT :: # */
    var EXPECTED_QUEUE_COUNT = 2322;
    /*@ readonly EXPECTED_HOLD_COUNT :: # */
    var EXPECTED_HOLD_COUNT = 928;

    /*@ readonly ID_IDLE :: # */
    var ID_IDLE       = 0;
    /*@ readonly ID_WORKER :: # */
    var ID_WORKER     = 1;
    /*@ readonly ID_HANDLER_A :: # */
    var ID_HANDLER_A  = 2;
    /*@ readonly ID_HANDLER_B :: # */
    var ID_HANDLER_B  = 3;
    /*@ readonly ID_DEVICE_A :: # */
    var ID_DEVICE_A   = 4;
    /*@ readonly ID_DEVICE_B :: # */
    var ID_DEVICE_B   = 5;
    /*@ readonly NUMBER_OF_IDS :: # */
    var NUMBER_OF_IDS = 6;
    /*@ readonly KIND_DEVICE :: # */
    var KIND_DEVICE   = 0;
    /*@ readonly KIND_WORK :: # */
    var KIND_WORK     = 1;
    /*@ readonly DATA_SIZE :: # */
    var DATA_SIZE = 4;

    /**
     * The task is running and is currently scheduled.
     */
    /*@ readonly STATE_RUNNING :: # */
    var STATE_RUNNING   = 0x00000000;

    /**
     * The task has packets left to process.
     */
    /*@ readonly STATE_RUNNABLE :: # */
    var STATE_RUNNABLE  = 0x00000001;

    /**
     * The task is not currently running.  The task is not blocked as such and may
     * be started by the scheduler.
     */
    /*@ readonly STATE_SUSPENDED :: # */
    var STATE_SUSPENDED = 0x00000002;

    /**
     * The task is blocked and cannot be run until it is explicitly released.
     */
    /*@ readonly STATE_HELD :: # */
    var STATE_HELD      = 0x00000004;

    /*@ readonly STATE_SUSPENDED_RUNNABLE :: # */
    var STATE_SUSPENDED_RUNNABLE = STATE_SUSPENDED | STATE_RUNNABLE;
    /*@ readonly STATE_NOT_HELD :: # */
    var STATE_NOT_HELD           = 0xFFFFFFFB //ORIG: ~STATE_HELD;

    export function testRichards() {
        for (var i =0; i< 50; i++) {
            runRichards();
        }
    }
    
    /**
     * The Richards benchmark simulates the task dispatcher of an
     * operating system.
     **/
    function runRichards() {
        var scheduler = new Scheduler(0,0,new Array<TaskControlBlock>(NUMBER_OF_IDS),null,null,-1);
        scheduler.addIdleTask(ID_IDLE, 0, null, COUNT);

        var queue = new Packet(null, ID_WORKER, KIND_WORK, 0);
        queue = new Packet(queue,  ID_WORKER, KIND_WORK, 0);
        scheduler.addWorkerTask(ID_WORKER, 1000, queue);

        queue = new Packet(null, ID_DEVICE_A, KIND_DEVICE, 0);
        queue = new Packet(queue,  ID_DEVICE_A, KIND_DEVICE, 0);
        queue = new Packet(queue,  ID_DEVICE_A, KIND_DEVICE, 0);
        scheduler.addHandlerTask(ID_HANDLER_A, 2000, queue);

        queue = new Packet(null, ID_DEVICE_B, KIND_DEVICE, 0);
        queue = new Packet(queue,  ID_DEVICE_B, KIND_DEVICE, 0);
        queue = new Packet(queue,  ID_DEVICE_B, KIND_DEVICE, 0);
        scheduler.addHandlerTask(ID_HANDLER_B, 3000, queue);

        scheduler.addDeviceTask(ID_DEVICE_A, 4000, null);

        scheduler.addDeviceTask(ID_DEVICE_B, 5000, null);

        scheduler.schedule();

        if (scheduler.queueCount !== EXPECTED_QUEUE_COUNT ||
            scheduler.holdCount !== EXPECTED_HOLD_COUNT) {
            var msg =
                "Error during execution: queueCount = " + scheduler.queueCount +
                ", holdCount = " + scheduler.holdCount + ".";
            throw new Error(msg); //TODO
        }
    }


    /**
     * A scheduler can be used to schedule a set of tasks based on their relative
     * priorities.  Scheduling is done by maintaining a list of task control blocks
     * which holds tasks and the data queue they are processing.
     * @constructor
     */
    class Scheduler {
        /*@ queueCount : number */
        public queueCount = 0;
        /*@ holdCount : number */
        public holdCount = 0;
        /*@ blocks : {IArray<MTaskControlBlock + null> | (len v) = NUMBER_OF_IDS} */
        public blocks : TaskControlBlock[] = new Array<TaskControlBlock>(NUMBER_OF_IDS);
        /*@ list : MTaskControlBlock + null */
        public list : TaskControlBlock = null;
        /*@ currentTcb : MTaskControlBlock + null */
        public currentTcb : TaskControlBlock = null;
        /*@ currentId : {number | -1<=v && v<NUMBER_OF_IDS} */
        public currentId:number = -1;

        /*@ new /\ (queueCount:number, 
                    holdCount:number, 
                    blocks:{IArray<MTaskControlBlock + null> | (len v) = NUMBER_OF_IDS}, 
                    list:MTaskControlBlock + null, 
                    currentTcb:MTaskControlBlock + null, 
                    currentId:{number | -1<=v && v<NUMBER_OF_IDS}) => Scheduler<M> */
                //\ () => Scheduler<M>
        constructor(queueCount?, holdCount?, blocks?, list?, currentTcb?, currentId?) {
            this.queueCount = queueCount;
            this.holdCount = holdCount;
            this.blocks = blocks;
            this.list = list;
            this.currentTcb = currentTcb;
            this.currentId = currentId;
        }

        /**
         * Add an idle task to this scheduler.
         * @param {int} id the identity of the task
         * @param {int} priority the task's priority
         * @param {Packet} queue the queue of work to be processed by the task
         * @param {int} count the number of times to schedule the task
         */
        /*@ addIdleTask : (this: MScheduler, 
                           id:natLT<NUMBER_OF_IDS>, 
                           priority:number, 
                           queue:MPacket + null, 
                           count:number) : void */
        public addIdleTask(id: number, priority: number, queue:Packet, count: number) {
            this.addRunningTask(id, priority, queue, new IdleTask(this, 0x00000001, count));
        }

        /**
         * Add a work task to this scheduler.
         * @param {int} id the identity of the task
         * @param {int} priority the task's priority
         * @param {Packet} queue the queue of work to be processed by the task
         */
        /*@ addWorkerTask : (this: MScheduler, 
                             id:natLT<NUMBER_OF_IDS>, 
                             priority:number, 
                             queue:MPacket + null) : void */
        public addWorkerTask(id:number, priority:number, queue:Packet) {
            this.addTask(id, priority, queue, new WorkerTask(this, ID_HANDLER_A, 0));
        }

        /**
         * Add a handler task to this scheduler.
         * @param {int} id the identity of the task
         * @param {int} priority the task's priority
         * @param {Packet} queue the queue of work to be processed by the task
         */
        /*@ addHandlerTask : (this: MScheduler, 
                              id:natLT<NUMBER_OF_IDS>, 
                              priority:number, 
                              queue:MPacket + null) : void */
        public addHandlerTask(id:number, priority:number, queue:Packet) {
            this.addTask(id, priority, queue, new HandlerTask(this, null, null));
        }

        /**
         * Add a handler task to this scheduler.
         * @param {int} id the identity of the task
         * @param {int} priority the task's priority
         * @param {Packet} queue the queue of work to be processed by the task
         */
        /*@ addDeviceTask : (this: MScheduler, 
                             id:natLT<NUMBER_OF_IDS>, 
                             priority:number, 
                             queue:MPacket + null) : void */
        public addDeviceTask(id:number, priority:number, queue:Packet) {
            this.addTask(id, priority, queue, new DeviceTask(this, null))
        }

        /**
         * Add the specified task and mark it as running.
         * @param {int} id the identity of the task
         * @param {int} priority the task's priority
         * @param {Packet} queue the queue of work to be processed by the task
         * @param {Task} task the task to add
         */
        /*@ addRunningTask : (this:MScheduler, 
                              id:natLT<NUMBER_OF_IDS>, 
                              priority:number, 
                              queue:MPacket + null, 
                              task:MTask) : void */
        public addRunningTask(id:number, priority:number, queue:Packet, task:Task) {
            this.addTask(id, priority, queue, task);
            var currentTcb = this.currentTcb;
            if (!currentTcb) throw new Error('This check should never fail'); // since addTask sets this.currentTcb
            currentTcb.setRunning();
        }

        /**
         * Add the specified task to this scheduler.
         * @param {int} id the identity of the task
         * @param {int} priority the task's priority
         * @param {Packet} queue the queue of work to be processed by the task
         * @param {Task} task the task to add
         */
        /*@ addTask : (this:MScheduler, 
                       id:natLT<NUMBER_OF_IDS>, 
                       priority:number, 
                       queue:MPacket + null, 
                       task:MTask) : {void | true} */
        public addTask(id:number, priority:number, queue:Packet, task:Task) {
            this.currentTcb = new TaskControlBlock(this.list, id, priority, queue, task);
            this.list = this.currentTcb;
            this.blocks[id] = this.currentTcb;
        }

        /**
         * Execute the tasks managed by this scheduler.
         */
        /*@ schedule : (this:MScheduler) : {void | true} */
        public schedule() {
            this.currentTcb = this.list;
            var currentTcb = this.currentTcb;
            while (currentTcb) {
                console.log("+");
                if (currentTcb.isHeldOrSuspended()) {
                    this.currentTcb = currentTcb.link;
                } else {
                    this.currentId = currentTcb.id;
                    this.currentTcb = currentTcb.run();
                }
                currentTcb = this.currentTcb;
            }
        }


        /**
         * Release a task that is currently blocked and return the next block to run.
         * @param {int} id the id of the task to suspend
         */
        /*@ release : (id:natLT<NUMBER_OF_IDS>) : {MTaskControlBlock + null | true} */
        public release(id:number) {
            var tcb = this.blocks[id];
            if (!tcb) return tcb;
            var currentTcb = this.currentTcb;
            if (!currentTcb) throw new Error("Illegal state");
            tcb.markAsNotHeld();
            if (tcb.priority > currentTcb.priority) {
                return tcb;
            } else {
                return currentTcb;
            }
        }


        /**
         * Block the currently executing task and return the next task control block
         * to run.  The blocked task will not be made runnable until it is explicitly
         * released, even if new work is added to it.
         */
        /*@ holdCurrent : (this:MScheduler) : MTaskControlBlock + null */
        public holdCurrent() {
            var currentTcb = this.currentTcb;
            if (!currentTcb) throw new Error("Illegal state");
            this.holdCount++; 
            currentTcb.markAsHeld();
            return currentTcb.link;
        }


        /**
         * Suspend the currently executing task and return the next task control block
         * to run.  If new work is added to the suspended task it will be made runnable.
         */
        /*@ suspendCurrent : () : MTaskControlBlock */
        public suspendCurrent () {
            var currentTcb = this.currentTcb;
            if (!currentTcb) throw new Error("Illegal state");
            currentTcb.markAsSuspended();
            return currentTcb;
        }

        /**
         * Add the specified packet to the end of the worklist used by the task
         * associated with the packet and make the task runnable if it is currently
         * suspended.
         * @param {Packet} packet the packet to add
         */
        /*@ queue : (this:MScheduler, packet: MPacket) : {MTaskControlBlock + null | true} */
        public queue(packet) {
            var t = this.blocks[packet.id];
            if (!t) return t;
            this.queueCount++;
            packet.link = null;
            var currentId = this.currentId;
            if (currentId === -1) throw new Error("Illegal state");
            packet.id = currentId;
            var currentTcb = this.currentTcb;
            if (!currentTcb) throw new Error("Illegal state");
            return t.checkPriorityAdd(currentTcb, packet);
        }
    }




    class TaskControlBlock {
        /*@ state : bitvector32 */
        private state = 0x00000000;
        /*@ link : MTaskControlBlock + null */
        public link;
        /*@ id : natLT<NUMBER_OF_IDS> */
        public id;
        public priority;
        /*@ queue : MPacket + null */
        public queue;
        /*@ task : MTask */
        public task;

        /**
         * A task control block manages a task and the queue of work packages associated
         * with it.
         * @param {TaskControlBlock} link the preceding block in the linked block list
         * @param {int} id the id of this block
         * @param {int} priority the priority of this block
         * @param {Packet} queue the queue of packages to be processed by the task
         * @param {Task} task the task
         * @constructor
         */
        /*@ new(link:MTaskControlBlock + null, 
                id:natLT<NUMBER_OF_IDS>, 
                priority:number, 
                queue:MPacket + null, 
                task:MTask) => TaskControlBlock<M> */
        constructor(link, id, priority, queue, task) {
            this.link = link;
            this.id = id;
            this.priority = priority;
            this.queue = queue;
            this.task = task;
            //ORIG:
            // if (!queue) {
                this.state = STATE_SUSPENDED;
            // } else {
            //     this.state = STATE_SUSPENDED_RUNNABLE;
            // }
        }

        /*@ setRunning : (this:MSelf) : {void | true} */
        public setRunning () {
            this.state = STATE_RUNNING;
        }

        /*@ markAsNotHeld : (this:MSelf) : {void | true} */
        public markAsNotHeld () {
            this.state = this.state & STATE_NOT_HELD;
        }

        /*@ markAsHeld : (this:MSelf) : {void | true} */
        public markAsHeld () {
            this.state = this.state | STATE_HELD;
        }

        public isHeldOrSuspended () {
            return (this.state & STATE_HELD) !== 0 || (this.state === STATE_SUSPENDED);
        }

        /*@ markAsSuspended : (this:MSelf) : {void | true} */
        public markAsSuspended () {
            this.state = this.state | STATE_SUSPENDED;
        }

        /*@ markAsRunnable : (this:MSelf) : {void | true} */
        public markAsRunnable () {
            this.state = this.state | STATE_RUNNABLE;
        }

        /**
         * Runs this task, if it is ready to be run, and returns the next task to run.
         */
        /*@ run : (this:MSelf) : {MTaskControlBlock + null | true} */
        public run () {
            //ORIG:
            // if (!(this.state === STATE_SUSPENDED_RUNNABLE)) {
            //     return this.task.run();
            // }
            var packet = this.queue;
            if (!packet) throw new Error("Illegal state: this.queue is null yet this.state is SUSPENDED_RUNNABLE");
            this.queue = packet.link;
            //ORIG:
            // if (!this.queue) {
            //     this.state = STATE_RUNNING;
            // } else {
            //     this.state = STATE_RUNNABLE;
            // }
            return this.task.run(packet);
        }

        /**
         * Adds a packet to the worklist of this block's task, marks this as runnable if
         * necessary, and returns the next runnable object to run (the one
         * with the highest priority).
         */
        /*@ checkPriorityAdd : (this:MTaskControlBlock, 
                                task:MTaskControlBlock, 
                                packet:MPacket) : MTaskControlBlock */
        public checkPriorityAdd (task, packet) {
            if (!this.queue) {
                this.queue = packet;
                this.markAsRunnable();
                if (this.priority > task.priority) return this;
            } else {
                this.queue = packet.addTo(this.queue);
            }
            return task;
        }

        public toString () {
            //TODO: explicit String call shouldn't be necessary
            return "tcb { " + String(this.task) + "@" + String(this.state) + " }";
        }

    }

    class Task {
        constructor() {}
        /*@ run : /\ (this:MSelf, packet: MPacket) : { MTaskControlBlock + null | true }
                  /\ (this:MSelf) : { MTaskControlBlock + null | true } */
        public run(packet:Packet) : TaskControlBlock {
            throw "Abstract method";
        }
    }

    class IdleTask extends Task {
        /*@ scheduler : MScheduler */
        public scheduler;
        /*@ v1 : bitvector32 */
        public v1;
        /*@ count : number */
        public count;
        /**
         * An idle task doesn't do any work itself but cycles control between the two
         * device tasks.
         * @param {Scheduler} scheduler the scheduler that manages this task
         * @param {int} v1 a seed value that controls how the device tasks are scheduled
         * @param {int} count the number of times this task should be scheduled
         * @constructor
         */
        /*@ new(scheduler:MScheduler, v1:bitvector32, count:number) => {IdleTask<M> | true} */
        constructor(scheduler, v1, count) {
            super();
            this.scheduler = scheduler;
            this.v1 = v1;
            this.count = count;
        }

        /*@ run : /\ (this:MSelf, packet: Packet<ReadOnly>) : { MTaskControlBlock + null | true }
                  /\ (this:MSelf) : { MTaskControlBlock + null | true } */
        public run(packet:Packet) : TaskControlBlock {
            this.count--;
            if (this.count === 0) return this.scheduler.holdCurrent();
            //ORIG:
            // if ((this.v1 & 1) === 0) {
            //     this.v1 = this.v1 >> 1;
                return this.scheduler.release(ID_DEVICE_A);
            // } else {
            //     this.v1 = (this.v1 >> 1) ^ 0xD008;
            //     return this.scheduler.release(ID_DEVICE_B);
            // }
        }

        public toString() {
            return "IdleTask";
        }
    }

    class DeviceTask extends Task {
        /*@ scheduler : MScheduler */
        public scheduler;
        /*@ v1 : MPacket + null */
        public v1 = null;

        /**
         * A task that suspends itself after each time it has been run to simulate
         * waiting for data from an external device.
         * @param {Scheduler} scheduler the scheduler that manages this task
         * @constructor
         */
        /*@ new /\ (scheduler:MScheduler, v1:MPacket + null) => {DeviceTask<M> | true} */
                //\ (scheduler:MScheduler) => {DeviceTask<M> | true} */
        constructor(scheduler, v1?) {
            super();
            this.scheduler = scheduler;
            this.v1 = v1;// if (arguments.length === 2) this.v1 = v1;
        }

        /*@ run : /\ (this:MSelf, packet: MPacket) : { MTaskControlBlock + null | true }
                  /\ (this:MSelf) : { MTaskControlBlock + null | true } */
        public run(packet:Packet) : TaskControlBlock {
            if (!packet) {
                var v1 = this.v1;
                if (!v1) return this.scheduler.suspendCurrent();
                var v = v1;
                this.v1 = null;
                return this.scheduler.queue(v);
            } else {
                this.v1 = packet;
                return this.scheduler.holdCurrent();
            }
        }

        public toString() {
            return "DeviceTask";
        }
    }

    class WorkerTask extends Task {
        /*@ scheduler : MScheduler */
        public scheduler;
        /*@ v1 : natLT<NUMBER_OF_IDS> */
        public v1;
        /*@ v2 : nat */
        public v2;
        /**
         * A task that manipulates work packets.
         * @param {Scheduler} scheduler the scheduler that manages this task
         * @param {int} v1 a seed used to specify how work packets are manipulated
         * @param {int} v2 another seed used to specify how work packets are manipulated
         * @constructor
         */
        /*@ new(scheduler:MScheduler, v1:natLT<NUMBER_OF_IDS>, v2:nat) => WorkerTask<M> */
        constructor(scheduler, v1, v2) {
            super();
            this.scheduler = scheduler;
            this.v1 = v1;
            this.v2 = v2;
        }

        /*@ run : /\ (this:MSelf, packet: MPacket) : { MTaskControlBlock + null | true }
                  /\ (this:MSelf) : { MTaskControlBlock + null | true } */
        public run(packet:Packet) : TaskControlBlock {
            if (!packet) {
                return this.scheduler.suspendCurrent();
            } else {
                if (this.v1 === ID_HANDLER_A) {
                    this.v1 = ID_HANDLER_B;
                } else {
                    this.v1 = ID_HANDLER_A;
                }
                (<Packet>packet).id = this.v1;
                (<Packet>packet).a1 = 0;
                for (var i = 0; i < DATA_SIZE; i++) {
                    this.v2++;
                    if (this.v2 > 26) this.v2 = 1;
                    packet.a2[i] = this.v2;
                }
                return this.scheduler.queue(packet);
            }
        }

        public toString() {
            return "WorkerTask";
        }
    }

    class HandlerTask extends Task {
        /*@ scheduler : MScheduler */
        public scheduler;
        /*@ v1 : MPacket + null */
        public v1 = null;
        /*@ v2 : MPacket + null */
        public v2 = null;

        /**
         * A task that manipulates work packets and then suspends itself.
         * @param {Scheduler} scheduler the scheduler that manages this task
         * @constructor
         */
        /*@ new /\ (scheduler:MScheduler, v1:MPacket + null, v2:MPacket + null) => {HandlerTask<M> | true} */
                //\ (scheduler:MScheduler) => {HandlerTask<M> | true} */
        constructor(scheduler, v1?, v2?) {
            super();
            this.scheduler = scheduler;
            // if (arguments.length === 3) {
                this.v1 = v1;
                this.v2 = v2;
            // }
        }

        /*@ run : /\ (this:MSelf, packet: MPacket) : { MTaskControlBlock + null | true }
                  /\ (this:MSelf) : { MTaskControlBlock + null | true } */
        public run(packet:Packet) : TaskControlBlock {
            if (packet) {
                if (packet.kind === KIND_WORK) {
                    this.v1 = packet.addTo(this.v1);
                } else {
                    this.v2 = packet.addTo(this.v2);
                }
            }
            var v1 = this.v1;
            if (v1) {
                var count = v1.a1;
                if (count < DATA_SIZE) {
                    var v2 = this.v2;
                    if (v2) {
                        var v = v2;
                        this.v2 = v2.link;
                        (<Packet>v).a1 = (<Packet>v1).a2[count];
                        (<Packet>v1).a1 = count + 1;
                        return this.scheduler.queue(v);
                    }
                } else {
                    var v = v1;
                    this.v1 = v1.link;
                    return this.scheduler.queue(v);
                }
            }
            return this.scheduler.suspendCurrent();
        }

        public toString() {
            return "HandlerTask";
        }
    }

    /* --- *
     * P a c k e t
     * --- */

    class Packet {
        /*@ a2 : {IArray<nat> | (len v) = DATA_SIZE} */
        public a2;

        /*@ link : MPacket + null */
        public link;
        /*@ id : natLT<NUMBER_OF_IDS> */
        public id;
        public kind:number;
        /*@ a1 : nat */
        public a1 = 0;
        /**
         * A simple package of data that is manipulated by the tasks.  The exact layout
         * of the payload data carried by a packet is not importaint, and neither is the
         * nature of the work performed on packets by the tasks.
         *
         * Besides carrying data, packets form linked lists and are hence used both as
         * data and worklists.
         * @param {Packet} link the tail of the linked list of packets
         * @param {int} id an ID for this packet
         * @param {int} kind the type of this packet
         * @constructor
         */
        /*@ new /\ (link:MPacket + null, id:natLT<NUMBER_OF_IDS>, kind:number, a1:nat) => Packet<M> */
                //\ (link:MPacket + null, id:natLT<NUMBER_OF_IDS>, kind:number)         => Packet<M> */
        constructor(link, id, kind, a1?) {
            this.a2 = new Array(DATA_SIZE);
            this.link = link;
            this.id = id;
            this.kind = kind;
            this.a1 = a1;
            // if (arguments.length === 4) this.a1 = a1;
        }

        /**
         * Add this packet to the end of a worklist, and return the worklist.
         * @param {Packet} queue the worklist to add this packet to
         */
        /*@ addTo : (this: MPacket, queue: MPacket + null) : MPacket */
        public addTo(queue:Packet) : Packet {
            this.link = null;
            if (!queue) return this;
            var next = queue;
            var peek = next.link;
            while (peek) {
                next = peek;
                peek = next.link;
            }
            (<Packet>next).link = this;
            return queue;
        }

        public toString() {
            return "Packet";
        }
    }
}