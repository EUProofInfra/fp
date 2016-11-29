module Sowpods where

{-
Taken from https://www.wordgamedictionary.com/sowpods/download/sowpods.txt and converted to Haskell syntax.


SOWPODS (Europe Scrabble Word List)

Did you know this is a European word list? You can find the US version here http://www.wordgamedictionary.com/twl06/download/twl06.txt

You can also find an extensive and larger English word list here http://www.wordgamedictionary.com/english-word-list/download/english.txt

---

Generated using the following Python program:

s = [w.strip() for w in open("sowpods.txt").readlines()]

'sowpods = read "[\\\n\\    {}]"'.format(
    ',\\\n\\    '.join('\\"{}\\"'.format(w) for w in s[6:]))


-}

sowpods :: [String]
sowpods = read $ "[\
\    \"aa\",\
\    \"aah\",\
\    \"aahed\",\
\    " {- ... etc ... -} ++ "\
\    \"zymurgies\",\
\    \"zymurgy\",\
\    \"zythum\",\
\    \"zythums\",\
\    \"zyzzyva\",\
\    \"zyzzyvas\",\
\    \"zzz\",\
\    \"zzzs\"]"
