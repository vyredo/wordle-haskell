
export class Nothing{}
export class Just<T> {
    value: T;
    constructor(n: T) { this.value = n}
}

export type Maybe<T> = Nothing | Just<T>