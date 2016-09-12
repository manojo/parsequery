# parsequery

[![Build Status](https://travis-ci.org/manojo/parsequery.svg?branch=master)](https://travis-ci.org/manojo/parsequery)

What is Parsequery?
-------------------

Parsequery is a parser combinator library. It uses
[staging](http://manojo.github.io/2015/09/02/staged-parser-combinators) to
remove composition overhead at compile time to produce efficient, fast parsers.
Its interface is almost similar to [Scala's parser
combinators](https://github.com/scala/scala-parser-combinators).


Example
-------

To benefit from staging, you wrap your parser declarations in the `optimise`
scope. Here is an example JSON parser:

```scala
  sealed abstract class JSValue
  case class JSObject(dict: List[(String, JSValue)]) extends JSValue
  case class JSArray(arr: List[JSValue]) extends JSValue
  case class JSDouble(d: Double) extends JSValue
  case class JSString(s: String) extends JSValue
  case class JSBool(b: Boolean) extends JSValue
  case object JSNull extends JSValue

  val jsonParser = optimise {

    def value: Parser[JSValue] = (
      obj |
      arr |
      stringLiteral.map(x => JSString(x)) |
      double.map(x => JSDouble(x)) |
      accept("null").map(_ => JSNull) |
      accept("true").map(_ => JSBool(true)) |
      accept("false").map(_ => JSBool(false))
    )

    def obj: Parser[JSValue] = (skipWs(accept('{')) ~>
      repsep(member, skipWs(accept(',')))
    <~ skipWs(accept('}'))) map { x => JSObject(x) }

    def arr: Parser[JSValue] = (skipWs(accept('[')) ~>
      repsep(value, skipWs(accept(',')))
    <~ skipWs(accept(']'))) map { x => JSArray(x) }

    def member: Parser[(String, JSValue)] =
      stringLiteral ~ (skipWs(accept(':')) ~> value)

    value
  }
```

To run this parser, you run it as follows:

```scala

val myReader = CharReader("""{"libname": "parsequery"}""".toArray)

jsonParser(myReader) match {
  case Success(res, rest) => println(res)
  case Failure(err, _)    => println(err)
}
```

You can find more examples in the test files [here](macros/src/test/scala/parsec/optimised/TestOptimisedParsers.scala)

How do I use it?
----------------

A snapshot is available on Sonatype. To use it with SBT, add the following lines
to your build:

```scala
libraryDependencies += "com.github.begeric" % "fastparsers_2.11" % "0.1-SNAPSHOT"
resolvers += Resolver.sonatypeRepo("snapshots")
```

How fast is it?
---------------

Check for yourself, by running the following two benchmarks:

```scala
> bench:testOnly parsequery.BooleansBenchmark
> bench:testOnly parsequery.JSONBenchmark
```

On these benchmarks we are currently about 2-3x faster than [FastParse](https://github.com/lihaoyi/fastparse).

More Information
----------------

The main goal of `parsequery` is to _systematically_ eliminate all intermediate
data structures that are creating when running a traditional parser combinator
program. Typically, parser combinators interleave _static_ composition of
parsers with  the _dynamic_ act of parsing itself, at runtime. The key insight
is that we can fully decouple the static parts from the dynamic one. This is
done by leveraging a technique known as [partial
evaluation](https://en.wikipedia.org/wiki/Partial_evaluation). To know more
about the ideas check out the these blog posts on optimising [list
pipelines](http://manojo.github.io/2015/02/19/staging-foldleft) and [parser
combinators](http://manojo.github.io/2015/09/02/staged-parser-combinators). A
more detailed blog post catered to the current implementation is to appear soon.

This implementation also heavily draws from a previous implementation, by the
man called [Eric Béguet](https://github.com/begeric), found
[here](https://github.com/begeric/FastParsers).
