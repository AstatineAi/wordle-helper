# Wordle Helper

A tool to assist solving Wordle puzzles by providing possible guesses.

## Usage

Run the application with one of the following commands:

```
./wordle-helper filter <guess1> <guess2> ...
./wordle-helper best <guess1> <guess2> ...
./wordle-helper new <guess1> <guess2> ...
```

- `filter`: Lists number of possible words based on previous guesses.
- `best`: Suggests the word that most likely to be the correct answer.
- `new`: Suggests new words likely to eliminate the most possibilities.

`words.txt` is downloaded from [Knuth's GraphBase list of 5-letter words](
    https://cs.stanford.edu/~knuth/sgb-words.txt).

## Web Frontend

A web frontend is available [here](https://icfp.dev/wordle-helper/).

This web app uses WebAssembly to run the Wordle helper logic in the browser.

Type in the words you have guessed, click the corresponding letter to change
its color (gray, yellow, green), and click buttons below to get suggestions.

**Known issue**: calculating "new words" with no prior guesses is slow in
the web version due to WebAssembly do not support multi-threading yet, and
the whole web page may get stuck. So start with some guesses to narrow down
the possibilities first. Or use the command-line version for that case.
