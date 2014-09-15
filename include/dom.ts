
/*************************************************************************
 *
 *      DOM API 
 *
 ************************************************************************/

interface Event {
    timeStamp: number;
    defaultPrevented: boolean;
    isTrusted: boolean;
//    currentTarget: EventTarget;
    cancelBubble: boolean;
//    target: EventTarget;
    eventPhase: number;
    cancelable: boolean;
    type: string;
//    srcElement: Element;
    bubbles: boolean;
    initEvent(eventTypeArg: string, canBubbleArg: boolean, cancelableArg: boolean): void;
    stopPropagation(): void;
    stopImmediatePropagation(): void;
    preventDefault(): void;
    CAPTURING_PHASE: number;
    AT_TARGET: number;
    BUBBLING_PHASE: number;
}

declare var Event: {
    prototype: Event;
    new(): Event;
    CAPTURING_PHASE: number;
    AT_TARGET: number;
    BUBBLING_PHASE: number;
}

declare var document: Document;


interface HTMLElement {

}

// https://github.com/Microsoft/TypeScript/blob/master/src/lib/dom.generated.d.ts
interface Document {

    documentElement: HTMLElement;

} 

declare var window: Window;

interface Window {

}
