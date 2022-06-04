
import {Just,Maybe, Nothing} from './Maybe'
import {evaluateGuess, getLinePr } from './Lib'

type EvaluateCharT = (c: string, i: number, r: string[]) => string;
const evaluateChar:EvaluateCharT = (char, idx, randomWord) => {
    if(char === randomWord[idx]) {
        return Green(char);
    }

    if(elem(char, randomWord)) {
        return Yellow(char);
    }

    return Gray(char);
}

const evaluateWord = (word: string, randomWord: string): string => {
    const chars = word.split('');
    const result = chars.map((char, idx) => evaluateChar(char, idx, randomWord.split('')));
    return result.join('');
}

const gameLoop = async (attempt: number, guesses: string[], randomWords: string): Promise<void> => {
    if (guesses.length >= 5) {
        console.log('Game Over!');
        return;
    }

    
    const guess = await getLinePr("Make a guess? \n");
    const result = evaluateGuess(guess);
    if (result instanceof Nothing) {
        console.log('Make sure the guess is 5 characters long');
        return gameLoop(attempt, guesses, randomWords);
    }

    if(new Just(randomWords).value === result.value) {
        console.log('You win!');
        return;
    }

    if(new Just(randomWords).value !== (result.value)) {
        const evalResult = evaluateWord(randomWords, result.value)
        console.log(evalResult);
        return gameLoop(attempt - 1, guesses.concat(result.value), randomWords);
    }
    console.log(`processing guess: `, result.value);

}

class GameState {
    randomWords:string = '';
    attempt: number = 0;
    guesses: string[] = [];
}

type MainT = () => void;
const main: MainT = async () => {
    const gameState = new GameState()
    gameState.randomWords = 'hello';
    gameLoop(gameState.attempt, gameState.guesses, gameState.randomWords)
    
}

main()