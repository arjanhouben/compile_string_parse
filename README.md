# compile_string_parse
playground to see if I could parse json at compile

## Why?
Because programming puzzles are fun!

I learned some tricks along the way, so that's nice

## Is it really all compile time?

Yes! https://godbolt.org/z/bE3P7s

Some functions are still instantiated, but could be removed by sprinkling some decltype statements around, since I have proven myself it worked, I sort of lost interest.
This is also true for parsing floating point numbers and I am sure there are a number of defects with regards to parsing JSON
