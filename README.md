# aang
A silly program to check whether a word consists solely of chemical element symbols.

For example,
```
$ aang wrestler
W Re S Tl Er
```
and sadly,
```
$ aang haskell
$
```

## Trivia
* About 12.054% of the English dictionary can be written entirely as successive chemical element symbols.
  This was checked against `/usr/share/dict/words` (99171 words) of which 11954 came out as positive.
* The name Aang comes from the television series 'Avatar - The Last Airbender' in which Aang is also called
  'Lord of the Elements' or 'Der Herr der Elemente' in German (pun intended).
* Other possible name choices were: Sherlock (based on 'elementary'), DearWatson (same, however there would be some confusion
  with IBM's 'watson'), elementum (because Latin always sounds cooler) and periodic (which is very boring).
