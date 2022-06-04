
import * as readline from 'node:readline';
import { stdin, stdout } from 'process';

const rl = readline.createInterface({
    input: stdin,
    output: stdout
});

type getLinePrT = (s: string) => Promise<string>;
export const getLinePr: getLinePrT = (s: string) => new Promise(resolve => {
    rl.question(s, (answer: string) => {
        resolve(answer);
    });
})
