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
 ----> now about fastparse is around 2x faster

 - folding into unit: works wonders. 4560 -> 11.25

 - moving to a bigger size (6600) and profiling again
