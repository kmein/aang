# aang [![Build Status](https://travis-ci.org/kmein/aang.svg?branch=master)](https://travis-ci.org/kmein/aang)
*A silly program to check whether a word consists solely of chemical element symbols.*

For example,
```
$ stack exec aang-exe -- wrestler
W Re S Tl Er
```
and sadly,
```
$ stack exec aang-exe -- haskell
$
```

## Trivia
* About 12.054% of the English dictionary can be written entirely as successive chemical element symbols.
  This was checked against `/usr/share/dict/words` (99171 words) of which 11954 came out as positive.
* For the German dictionary `/usr/share/dict/ngerman` (339099), the percentage
  amounts to around 8.333%, that is 28257 words.
* The name Aang comes from the television series *Avatar – The Last Airbender* in which Aang, the main character, 
  is the *Lord of the Elements* (pun intended).
  
![That's him: Aang](https://vignette.wikia.nocookie.net/nicktheultimaswordwielder/images/f/f1/Aang-2-.jpg)
