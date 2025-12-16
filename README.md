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

Make sure to have a `words.txt` file in the same directory
containing the list of valid words.

e.g. [Knuth's GraphBase list of 5-letter words](
    https://cs.stanford.edu/~knuth/sgb-words.txt).
