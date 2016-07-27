NOTES
-----

So far, things are quite slow:

Alternatives tried, for parsing a sequence of booleans:

 - hoisting out a string into a final val: does not have much
 impact just yet

 - using parseMany to help with concatenation: does not have much
 impact just yet

 - using a string parser that gets an Array[Char]: no impact yet

 - folding into ArrayBuffer: gets the first order of magnitude ()
 - folding into ArrayBuffer, using += instead of :+ the next order of magnitude
 - but actually, the problem is with the :+ function, which is generic, and
 applies to "immutable" stuff.

 ----> now fastparse is around 2x faster

 - folding into unit: works wonders. 4560 -> 11.25

 - moving to a bigger size (6600) and profiling again

 - def ws = repFold(singleSpace | CRLF), if replaced by
   def ws = repFold(acceptIf(x => x == ' ' || x == '\n')) shaves more time
   again. Can this be done automatically by staging?

 ---->

  - specialise final parser, even if it ain't named.
    - since we must return a Parser[T], if the final parser is not named we create
     a new unnamed parser that we stage and functionalise, and return this name
     as the final parser.
    - hence the normalised form of a parser block is to have a collection of parsers
    ending with a final, named one.
  - more than one production
  - json parser <-- check byte size of this guy
  -

---->
