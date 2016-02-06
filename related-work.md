Related work
------------


D. Colazzo, G. Ghelli, and C. Sartiani. _[Typing massive JSON datasets](http://workshops.inf.ed.ac.uk/xldi2012/colazzo_xldi.pdf)_, XLDI 2012
(International workshop on cross-model language design and implementation)

**Summary:** They develop a type inference algorithm for massive JSON datasets.
For this, they use union types and subtyping.

**How it relates:** ???

--

P. Buneman and A. Ohori. _[Polymorphism and type inference in database
programming](http://dl.acm.org/citation.cfm?id=227609)_, ACM Transactions on DB
Systems 1996.

**Summary:** Partially dynamic records.

**How it relates:** ???

--

A. Ohori and  K. Ueno. _[Making Standard ML a practical database programming
langauge](http://www.pllab.riec.tohoku.ac.jp/papers/icfp2011OhoriUenoAuthorVersion.pdf)_,
ICFP 2011.

**Summary**: Record-based approach to integrate SQL into Standard ML?

**How it relates** ???

--

H. Hosoya and B. C. Pierce. _[XDuce: A Statically Typed XML Processing
Language](http://dl.acm.org/citation.cfm?id=767195)_. ACM Transactions on
Internet Technology, 2003

**Summary**: XDuce provides static typing for XML. XDuce supports flexible form
of types, namely regular expression types which may also be recursive. Regular
expression types could easily be used for typed programming with JSON objects,
and they support a principled treatment of optional fields. Moreover, XDuce
presents a pattern matching language which has also been formalized for XDuce.

**How it relates** ???

--

V. Benzaken, G. Castagna, and A. Frisch. _[CDuce: An XML-Centric General-Purpose Language](http://www.cduce.org/papers/cduce-design.ps.gz)_, ICFP 2003

Or perhaps look at the paper _Semantic Subtyping_, LICS 2002. Not sure which is
best to cite.

**Summary** XDuce as presented does not have first-class functions. However, this has been
addressed in CDuce. CDuce extends XDuce with first-class functions, among other
things.

**How it relates** ???

--

