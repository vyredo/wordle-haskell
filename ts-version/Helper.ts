import { Just, Maybe, Nothing } from "./Maybe";
import chalk from 'chalk';

type evaluateGuessT = (guess: string) => Maybe<string>;
export const evaluateGuess: evaluateGuessT = (guess) => {
    if(guess.trim().length !== 5) {
        return new Nothing();
    }

    return new Just(guess)
}

export const Gray = chalk.gray
export const Yellow = chalk.yellow
export const Green = chalk.green

export const elem = (a:string, b:string[]) =>  b.find(x => x === a) !== undefined;
