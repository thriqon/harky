Harky
=====

Harky is a simple and self-contained parser for Markdown. It interprets the
control characters and emits HTML code suitable for the WWW.

Introduction
------------

Markdown was developed by John Gruber as a plaintext-to-HTML conversion
tool. Ordinary plain text files (as produced regularly by coders)
containing documentation, blog posts, or whole books are enriched using
some control characters that are later parsed by the Markdown processor
and translated into their corresponding HTML tags. The whole point of
Markdown is that these enriched files have very little formatting
overhead and are visually pleasing even in their original version.
For example, a headline can be created by underlining it with a line
of dashes. At least since the rise of Github and their promotion of a
`README.md` for every project Markdown has become the de facto standard
for plain text formatting. 

This parser is written in Haskell using the standard libraries. Using
Haskell provides the ability to use monads with first-class syntactic
support to compose bits of functionality trivially. In the following program,
we will define the Parser monad. It can be understood to be just like
a marble run. We start with basic building blocks and combine those
repeatedly to create something bigger. In the end, when the code is
executed, a marble is dropped into the top-level block and it finds its
way through the whole construct. The metaphor is limited, though: There
will be blocks that run the marble through loops, or through different blocks
until one of then succeeds. The possibility of failure is deeply ingrained
into monads.

The strength of this 'monadic parsing' is composability. Each building
block can be taken and assembled into something even greater. The final
Markdown document parser could be used to build a parser for ZIP archives
containing Markdown files, for example, without changes. Some of this
can be implemented using Perl regular expressions as well, even composition
is possible. Nested application of parsers, the possibility to execute
arbitrary code during the parsing, and an expressive DSL as executable code
however is simpler with this approach.

This file itself serves as the executable Haskell code (using Literate
Haskell).

The remainder of the program is structured as follows: First, data
structures powering a Markdown document are defined. Afterwards the parsing
DSL is created and explained. Using this DSL a concrete parser for Markdown
is created.  The last chapter is dedicated to emittjng HTML from the data
structures.  The code to run the program and test cases can be found in the
appendix.

Due to technical limitations, options and imported modules have to come
first.  These options make the compiler treat *all* warnings as errors,
forcing to write clean code. The language extension `InstanceSigs` allows to
write the type signatures in instance declarations. It does not increase the
expressiveness of the language, but in the opinion of the author it aids in
understanding of the code.

> {-# OPTIONS_GHC -Wall -Werror #-}
> {-# LANGUAGE InstanceSigs #-}

This module exports two main methods, one for
the 'production' behaviour and one for tests.
It is called `Main` as this is file is
directly executable.

> module Main (main, testMain) where

We are using a great deal of modules from
the standard library here, with the exception
of the `bytestring` package.

> import Prelude
> import Control.Monad
> import Control.Applicative
> import Data.Either (isLeft)
> import qualified Data.ByteString.Lazy.Char8 as C
> import Data.Foldable (asum)

For testing we are using Tasty with SmallCheck
and HUnit:

> import Test.Tasty
> import Test.Tasty.SmallCheck as SC
> import Test.Tasty.Golden     as GC
> import Test.Tasty.HUnit

Markdown
--------

As a first step, we will define the data structures comprising a Markdown
document. Parsing and emitting is handled separately in later chapters.

The document containing all content is merely a list of `Snippets`, i.e.
top-level objects. These objects are defined below.  For debugging purposes it
is helpful to automatically derive instances for equality testing and value
printing:

> newtype Document = Document [Snippet]
>   deriving (Eq, Show)

Each of the `Snippet`s can be a different kind of
element, for example a headline or a paragraph.
In Haskell, this is encoded using sum types:

> data Snippet = Headline Int [Chunk]
>              | HorizontalLine
>              | UnorderedList [[Chunk]]
>              | Blockquote [Snippet]
>              | Paragraph [Chunk]
>   deriving (Show, Eq)

A Chunk is either an unformatted or a formatted
piece of text. When shown, they are interspersed
with some controlling commands to format the text.

> data Chunk = PlainText String
>            | StrongEmphasisChunk [Chunk]
>            | EmphasisChunk [Chunk]
>            | CodeChunk String
>            | LinkChunk [Chunk] String
>   deriving (Eq, Show)

The Generic Parser
------------------

The language Haskell does not offer a built-in parser,
but is quite easy to implement a fully functioning
parser library using functional features.

The following implementation heavily borrows
from the [Nanoparsec][1] by Stephen Diehl.

A parser is something that can parse:

> newtype Parser a = Parser { parse :: String -> [(a, String)]}

The function defined using record syntax
takes a string and returns a list of parsing results combined with the unparsed suffix of the string. It is easier to understand with the
accompanying `runParser` function, or an error message:

> runParser :: Parser a -> String -> Either String a
> runParser p s = interpret $ parse p s
>   where interpret [(result, "")] = Right result
>         interpret [(_, s')]       = Left $ "Parser did not consume entire input, leftover is " ++ s'
>         interpret _              = Left "General parser error."

This function runs the parser on the given input and makes sure
that the entire input is consumed. Any other result is an error.
An incomplete parsing may indicate an incomplete structure
on the end of the stream and should therefore be considered an error.

The `Parser a` can be instantiated as a `Functor`
(allowing the use of the mighty `fmap`):

> instance Functor Parser where
>   fmap f (Parser p) = Parser p'
>     where p' s = [(f a, b) | (a, b) <- p s]

The `fmap` function generalizes the widely known `map` operating on lists
to mapping over any Functor. In this instance, it provides a parser that
first uses the original parser, and then transforms the result, which is
returned.

The `Parser` is also an instance
of `Applicative`. This means - roughly speaking - that contained functions
can be applied to parameters:

> instance Applicative Parser where
>   pure :: a -> Parser a
>   pure x = Parser $ \s -> [(x, s)]
>   (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
>   (Parser pf) <*> (Parser pa) = Parser $ \s -> [(f a, rs2) | (f, rs1) <- pf s, (a, rs2) <- pa rs1]

The `pure` function builds a parser that just injects the argument, and does
not consume input. It can be used to programmatically simulate parsing of
certain tokens without them being present in the stream. The second function
allows applying a function *contained in a parser* transforming the second
argument.

The `Parser` is also a fully-fledged `Monad` (and later `MonadPlus`):

> instance Monad Parser where
>   p >>= f = Parser $ \s -> [r | (a, s') <- parse p s, r <- parse (f a) s']
>   return = pure

The first function is the so-called *bind* function. It combines a parser
with a function, and returns a parser that returns the result of the
first parser transformed by the second function. It is implicitly used
any time we use the do notation in Haskell. Tge `return` function wraps
the given value in a monad and is (here) equivalent to `pure`.

The strength of 'Monadic Parsing' stems from the possibilities we get by
making the `Parser` an instance of `Alternative` and `MonadPlus`. These two
type classes do *almost* the same, but the former is based on the
`Applicative` class while the latter derives from `Monad`. Both encode the
rules and possibilities of operations similar to simple addition.

> instance Alternative Parser where
>   empty :: Parser a
>   empty = Parser $ const []
>   (<|>) :: Parser a -> Parser a -> Parser a
>   a <|> b = Parser $ \s -> case parse a s of
>     []  -> parse b s
>     res -> res

The value returned by `empty` is expected to be the
neutral element with respect to `<|>`. Additionally,
the `<|>` operation has to fulfill associativity. Thanks to these two
conditions the two operations form a *monoid*. These *laws* are tested like this:

> tgAlternative :: TestTree
> tgAlternative = testGroup "Alternative is a monoid"
>   [ SC.testProperty "right neutral" (\c -> let s = [c] in runParser (item <|> empty) s == runParser item s)
>   , SC.testProperty "left  neutral" (\c -> let s = [c] in runParser (empty <|> item) s == runParser item s)
>   , SC.testProperty "associative" (\a -> let s = (a : "") in
>       runParser (item <|> (item <|> item)) s == runParser ((item <|> item) <|> item) s)
>   ]

> instance MonadPlus Parser where
>   mzero = empty
>   mplus a b = Parser $ \s -> parse a s ++ parse b s

The `mplus` function concatenates the results of two parsers.

Now, we can define the first 'real' parsers.  The `item` parser is very
simple: It parses the first character of the given string and returns the
remainder unparsed.  It is the basis of all other parsers working on
characters.

> item :: Parser Char
> item = Parser charParser
>   where charParser (c : cs) = [(c, cs)]
>         charParser _ = []

Its behaviour is easy to specify:

> tgItem :: TestTree
> tgItem = testGroup "item"
>   [ SC.testProperty "parses char" $ \c -> runParser item [c] == Right c
>   , testDoesNotParse "empty string fails" item ""
>   ]

Combinators
~~~~~~~~~~~

Combinators are used to combine or *compose*
different parsers into a new one with added
functionality. From these functions stems
the expressive power of monadic parsing.

When building parsers for complex structures it is often neccessary to
express the notion of repetition.  Usually, there are two kinds: Zero to
many or One to Many.  We will express those two cases with `many` and
`some`, respectively.  They are provided by the standard library in terms of
the `Alternative` instance.

> tgSomeMany :: TestTree
> tgSomeMany = testGroup "some/many"
>   [ testGroup "many"
>     [ testParsesAs     "'abc'" (many item) "abc" "abc"
>     , testParsesAs     "''"    (many item) ""    ""
>     ]
>   , testGroup "some"
>     [ testParsesAs     "'abc'" (some item) "abc" "abc"
>     , testDoesNotParse "''"    (some item) ""
>     ]
>   ]

Later on, we will need a parser that only accepts characters that fulfil some
predicate. We can define `satisfy` in terms of the above functions. But first,
lets specify its behavior:

> tgSatisfy :: TestTree
> tgSatisfy = testGroup "satisfy"
>   [ testDoesNotParse "reject char" (satisfy $ const False) "a"
>   , testParsesAs     "accept char" (satisfy $ const True)  "a" 'a'
>   ]

And this function fulfils that spec:

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = item >>= withPredicate
>   where withPredicate c
>           | p c       = pure c
>           | otherwise = empty

A combination of `satisfy` and `some` is `someTill`. It uses one parses repeatedly and returns the result
as list, but only until the other parser succeeds. This combinator can be used to parse items that
are somehow bounded by other tokens, for example quoted strings or comments.

> tgSomeTill :: TestTree
> tgSomeTill = testGroup "someTill"
>   [ testParsesAs "quoted string"       quotedString "\"asd\""              "asd"
>   , testDoesNotParse "empty" (someTill item $ char ' ') ""
>   ]
>     where quotedString = char '"' >> someTill item (char '"')

> someTill :: Parser a -> Parser boundary -> Parser [a]
> someTill a b = liftA2 (:) a scan
>   where scan = (b *> pure []) <|> liftA2 (:) a scan

A nice simple combinator that will be useful later on is `oneOf`. It takes a list of parsers and returns the first positive result:

> tgOneOf :: TestTree
> tgOneOf = testGroup "oneOf"
>   [ testParsesAs     "with pure"  (oneOf [empty, pure 'a', pure 'b']) "" 'a'
>   , testDoesNotParse "only empty" (oneOf [empty, empty, empty]) "asd"
>   ]

The 'implementation' of this function is trivial: There is already a function in the standard library
that provides this behavior, namely `asum`. We can just use this implementation because our `Parser`
is an instance of `Alternative`.

> oneOf :: [Parser a] -> Parser a
> oneOf = asum

Some specific character is parsed using the `char` parser. It only succeeds if the next item is the given character, and fails otherwise.

> tgChar :: TestTree
> tgChar = testGroup "char"
>   [ testParsesAs     "match"    (char 'a') "a" 'a'
>   , testDoesNotParse "no match" (char 'a') "b"
>  ]

> char :: Char -> Parser Char
> char = satisfy . (==)

On top of `char` we define `string`, parsing the given sequence of characters.

> tgString :: TestTree
> tgString = testGroup "string"
>   [ testParsesAs "matching" (string "hi") "hi" "hi"
>   , testDoesNotParse "no match" (string "hi") "bye"
>   ]

The function `mapM` is similar to `map`, but it works in a monadic context.

> string :: String -> Parser String
> string = mapM char

The `lookAhead` combinator applies
the given parser and returns the
result, but does not consume the
input from the original stream.
It is comparable to `peek` in
network code.

> tgLookAhead :: TestTree
> tgLookAhead = testGroup "lookAhead"
>   [ testParsesAs "does not consume" consumptionTester "a.." ("a..", 'a', "..")
>   , testDoesNotParse "propagates failure" (lookAhead $ some item) ""
>   , testParsesAs "succeeds on not-eoi" notEoiTester "a.T" ('a', 'a')
>   ]
>     where consumptionTester = do
>                                 a <- lookAhead $ many item
>                                 b <- item
>                                 c <- many item
>                                 return (a,b,c)
>           notEoiTester      = do
>                                 a  <- lookAhead item
>                                 a' <- item
>                                 _  <- many item
>                                 return (a, a')


> lookAhead :: Parser a -> Parser a
> lookAhead p = Parser la
>   where la s = case parse p s of
>                  [(x, _)] -> [(x, s)]
>                  _        -> []

The `runP` combinator is special: Although it lives in the Parser monad
and has the ability to consume characters, it has a different scope.
It takes a parser and input data (a string) and runs this parser within
the failure context of the original monad.

This allows building parsers that modify the input for nested parsers, for
example to strip away leading comment indicators in source code. The nested
parser tasked with special markup instructions (e.g, bold text or links)
does not have to care at all about these comment indicators.

> tgRunP :: TestTree
> tgRunP = testGroup "runP"
>   [ testParsesAs "with empty" (runP item "a") "" 'a'
>   , testParsesAs "combination" (do{ a <- runP item "a"; b <- item ; return  (a, b)}) "b" ('a', 'b')
>   , testDoesNotParse "incomplete parsing" (runP item "abc") ""
>   , testDoesNotParse "no match" (runP (char 'a') "b") ""
>   ]

> runP :: Parser a -> String -> Parser a
> runP p s = Parser $ \s' -> case parse p s of
>                              [(x, "")] -> [(x, s')]
>                              _         -> []

The End-of-Input parser only succeeds when there is no more data to parse,
and then parses the empty string. In any other case, it fails. It is helpful
in combination with `someTill`, for example.

> eoi :: Parser String
> eoi = Parser eoip
>   where eoip "" = [("", "")]
>         eoip _  = []


This concludes the general parsing functions.  The next chapter will use these
primitives to build a Markdown parser.

The Markdown Parser
-------------------

Again, we take a top-down
approach. First of all, we
define a parser that parser
a whole document. A
document is a list of
snippets, and even the empty
list is valid:

> document :: Parser Document
> document = Document <$> many snippet

A snippet is one of the known snippet types. Each registered type is tried in
the following order, and the first to succeed defines the interpretation. In
this list, the order matters!  The input `## headline` can be interpreted as a
second-level headline with text 'headline', but it also fulfills all criteria
for a paragraph. This parser also takes care of any subsequent empty lines
between snippets (they are ignored).

> snippet :: Parser Snippet
> snippet = oneOf
>   [ atxHeadlines
>   , setextHeadlines 
>   , blockquote
>   , list
>   , horizontalLine
>   , paragraph
>   ] >>= ignoreSubsequentEols
>     where ignoreSubsequentEols dat = many eol >> return dat

A line-ending is either a newline
`U+000A` (Linux systems), a carriage
return `U+000D` (Macintosh) or a
carraiage return followed by newline
`U+000DU+000A`. By testing them in
the order below we can make sure to
parse all variants.

> eol :: Parser String
> eol = oneOf $ map string ["\n", "\r\n", "\r"]

There are two different kinds of headlines in Markdown: ATX and setext. ATX
headlines are single-line and prefixed with a number of hashes (`#`). A single
hash means a first-level heading, two hashes a second-level heading. Up to six
hashes are supported, mapping to the `h1` to `h6` tags in HTML.

> atxHeadlines :: Parser Snippet
> atxHeadlines = oneOf [atxHeadline n | n <- [1..6]]

> atxHeadline :: Int -> Parser Snippet
> atxHeadline level = do
>   _ <- string prefix
>   _ <- char ' '
>   l <- item `someTill` eol
>   pl <- runP chunks l
>   return $ conv pl
>     where prefix = replicate level '#'
>           conv   = Headline level

Setext headlines use a line of heading and below that a line of a set of
underlining characters. A first-level headline is denoted by equal signs
(`=`) and second-level by dashes (`-`). A common addition is a third level
denoted by tildes (`~`). The exact number of characters does not matter,
as long as it's greater than zero.

> setextHeadlines :: Parser Snippet
> setextHeadlines = oneOf
>   [ setextHeadline '=' 1
>   , setextHeadline '-' 2
>   , setextHeadline '~' 3
>   ]

> setextHeadline :: Char -> Int -> Parser Snippet
> setextHeadline uline level = do
>   l <- item `someTill` eol
>   _ <- underlining
>   Headline level <$> runP chunks l
>     where underlining = char uline `someTill` eol

A thematic break is indicated in Markdown by a sequence of dashes,
asterisks or underscores (`-*_`). The character does not matter,
but it is not possible to mix them. Spaces are okay anywhere.

> horizontalLine :: Parser Snippet
> horizontalLine = oneOf [hrWith (char x) | x <- "-*_"] >> return HorizontalLine
>   where hrWith c = spaces >> c
>                      >> spaces >> c
>                      >> spaces >> c
>                      >> many (c <|> space)
>         spaces   = many space
>         space    = char ' '

A blockquote contains a list of snippets, and each line
has to be prefixed by a `> `. Inside a blockquote anything is fair game,
even other blockquotes.
 
> blockquote :: Parser Snippet
> blockquote = some line >>= \ls -> fmap Blockquote $ runP (some snippet) $ unlines ls
>     where line = oneOf
>             [ string "> " >> (someTill item eol <|> empty)
>             , string ">"  >> eol
>             ]

A list is indicated by asterisks '*' at the beginning of a line. Each line is
parsed by itself.

> list :: Parser Snippet
> list = UnorderedList <$> (some li >>= mapM (runP chunks))
>   where li = string "* " >> someTill item eol

A paragraph is simple: It is line after line of
text, delimited by an empty line. This is
encoded using `someTill`, requring at least one
non-eol character per line.

> paragraph :: Parser Snippet
> paragraph = lsp >>= parsed
>   where lsp       = some $ someTill notEol (eol <|> eoi)
>         notEol    = satisfy (\x -> x /= '\r' && x /= '\n')
>         parsed ls = fmap Paragraph $ runP chunks $ unwords ls

The next section deals with chunks. In
combination they form a hierarchical construct
of the text and its formatting. Similar to
snippets, the order in the following list
matters! Every strong emphasized chunk is an
emphasized chunk as well, but not the other
way round.

> tgChunks :: TestTree
> tgChunks = testGroup "chunks"
>   [ testParsesAs "plain chunk" chunks "asd" [PlainText "asd"]
>   , testParsesAs "single char" chunks "."[PlainText "."]
>   , testParsesAs "chunk with emphasis" chunks "asd *dsa*" [PlainText "asd ", EmphasisChunk [PlainText "dsa"]]
>   , testParsesAs "chunk with code" chunks "asd `code`" [PlainText "asd ", CodeChunk "code"]
>   ]

This parser delegates to the actual chunk parsers:

> chunks :: Parser [Chunk]
> chunks = some $ oneOf
>   [ strongEmphasizedChunk
>   , emphasizedChunk
>   , codeChunk
>   , linkChunk
>   , plainChunk
>   ]

This function is rather difficult to understand. It parses a chunk
delimited by the first parameter, and transforms characters in between
if there is a second occurrence of the delimiter. Otherwise, the delimiter
is just returned with the rest of the data as a plain text. The second
case happens when an emphasized chunk is not properly closed (intentional or not).

> bounded :: Parser String -> (String -> Parser Chunk) -> Parser Chunk
> bounded b = bounded2 b b

> bounded2 :: Parser String -> Parser String -> (String -> Parser Chunk) -> Parser Chunk
> bounded2 b1 b2 onSuccess = b1 >>=
>   \ld -> item `someTill` lookAhead (b2 <|> eoi) >>=
>     \dat -> (b2 >> onSuccess dat) <|>
>       return (PlainText (ld ++ dat))

Emphasis chunks are delimited using asterisk or dashes, strongly emphasized chunks
with these characters twice.

> emphasisBoundary :: Parser String
> emphasisBoundary = oneOf [string "*", string "_"]

> strongEmphasisBoundary :: Parser String
> strongEmphasisBoundary = oneOf [string "**", string "__"]

> emphasizedChunk :: Parser Chunk
> emphasizedChunk = bounded emphasisBoundary (\dat -> EmphasisChunk <$> runP chunks dat)

> strongEmphasizedChunk :: Parser Chunk
> strongEmphasizedChunk = bounded strongEmphasisBoundary (\dat -> StrongEmphasisChunk <$> runP chunks dat)

Inline code is delimited by backticks andd contains only plain text.

> codeChunk :: Parser Chunk
> codeChunk = fmap CodeChunk $ codeDelimiter >> item `someTill` codeDelimiter

> codeDelimiter :: Parser String
> codeDelimiter = string "`"

Links have two components: The text and the
href. The text is enclosed in `[]`, the href in
`()`. Inside the text other formatting
instructions are allowed.

> linkChunk :: Parser Chunk
> linkChunk = bounded2 (string "[") (string "]") oSText
>   where oSText txt = runP chunks txt >>= hrefP
>         hrefP txtC = bounded2 (string "(") (string ")") osH
>           where osH h = return $ LinkChunk txtC h

> linkDelimiter :: Parser String
> linkDelimiter = string "["

A plain chunk is text until either the input is consumed or one of the boundaries occurs.

> plainChunk :: Parser Chunk
> plainChunk = PlainText <$> chk
>   where chk = someTill item eoc
>         eoc = eoi <|> lookAhead (oneOf [emphasisBoundary, strongEmphasisBoundary, codeDelimiter, linkDelimiter])


The HTML Emitter
----------------

This part is simple. We only need to define what emitting HTML means,
and then define concrete code for each supported Snippet and Chunk.
Each emittable data structure shall be an instance of `Hypertextable`:

> class Hypertextable x where
>   showHTML :: x -> String

The first data structure we instantiate is `Document`. It transforms
all its snippets into HTML and concatenates them. For readability
an newline character `\n` is added, although this is not technically
needed:

> instance Hypertextable Document where
>   showHTML (Document snippets) = concatMap ((++ "\n") . showHTML) snippets

The different snippets and chunks use a common way to render: They surround
something with HTML tags. This helper method provides an abstract way for this:

> surroundedByTag :: String -> String -> String
> surroundedByTag tagName s = openTag ++ s ++ closingTag
>   where tag t      = "<" ++ t ++ ">"
>         openTag    = tag tagName
>         closingTag = tag ( '/' : tagName)

> showHTMLChunks :: [Chunk] -> String
> showHTMLChunks = concatMap showHTML

> instance Hypertextable Chunk where
>   showHTML (PlainText chk)   = chk
>   showHTML (EmphasisChunk chk) = surroundedByTag "em" $ showHTMLChunks chk
>   showHTML (StrongEmphasisChunk chk)   = surroundedByTag "strong" $ showHTMLChunks chk
>   showHTML (CodeChunk str)   = surroundedByTag "code" str
>   showHTML (LinkChunk chk href) = "<a href=\"" ++ href ++ "\">" ++ showHTMLChunks chk ++ "</a>"


This function is easiliy testable:

> tgShowHTMLChunks :: TestTree
> tgShowHTMLChunks = testGroup "showHTML Chunks"
>   [ SC.testProperty "plain" (\s -> showHTML (PlainText s) == s)
>   , SC.testProperty "emphasis" (\s -> showHTML (EmphasisChunk [PlainText s]) == "<em>" ++ s ++ "</em>")
>   , SC.testProperty "strong" (\s -> showHTML (StrongEmphasisChunk [PlainText s]) == "<strong>" ++ s ++ "</strong>")
>   , testCase "nested" (showHTML (
>       StrongEmphasisChunk
>         [ PlainText "hello"
>         , EmphasisChunk
>             [ StrongEmphasisChunk [PlainText "world"]
>             ]
>         , PlainText "!"
>         ])
>       @?= "<strong>hello<em><strong>world</strong></em>!</strong>")
>   ]

> instance Hypertextable Snippet where
>   showHTML (Paragraph chks) = surroundedByTag "p" (showHTMLChunks chks)
>   showHTML (Headline n chks) = surroundedByTag ('h' : show n) $ showHTMLChunks chks
>   showHTML (Blockquote snips) = "<blockquote>\n" ++  contents ++ "</blockquote>"
>     where contents = concatMap ((++"\n") . showHTML) snips
>   showHTML (UnorderedList lis) = surroundedByTag "ul" contents
>     where contents = concatMap (surroundedByTag "li" . showHTMLChunks) lis
>   showHTML HorizontalLine    = "<hr />"

Let's add some tests for the correct emission:

> tgShowHTMLSnippets :: TestTree
> tgShowHTMLSnippets = testGroup "showHTML Snippets"
>   [ testCase "p" $ "<p>hello</p>" @=? showHTML (Paragraph [PlainText "hello"])
>   , testCase "h1" $ "<h1>hello</h1>" @=? showHTML (Headline 1 [PlainText "hello"])
>   , testCase "h2" $ "<h2>hello</h2>" @=? showHTML (Headline 2 [PlainText "hello"])
>   , testCase "blockquote" $ "<blockquote>\n<p>hello</p>\n</blockquote>" @=? showHTML (Blockquote [Paragraph [PlainText "hello"]])
>   , testCase "ul" $ "<ul><li>hello</li><li>world</li></ul>" @=? showHTML (UnorderedList [[PlainText "hello"], [PlainText "world"]])
>   ]

The Main Program
----------------

The harky function parses the given document and either returns an error
or an HTML representation.

> harky :: String -> Either String C.ByteString
> harky src = do
>   res <- runParser document src
>   return $ C.pack $ showHTML res

When running the main function, i.e. when this program is executed,
data on standard input is parsed and emitted on standard out.

> main ::IO ()
> main = getContents >>= either fail return . harky >>= C.putStr

In this program we have shown how straightforward the implementation of a
general parser library in Haskell is, and used this embedded library as an DSL
to build a Markdown parser and HTML emitter.  From here on it is easy to
continue. Possible next steps are the introduction of more features from
Markdown, for example idented and fenced code blocks.

Appendix
--------

There is a set of 'golden files' that is used to test
the parser. Each of the test cases provides a fixture and a result file.
The fixtures reside in a `.md`, while the golden results are stored in
`.html`. These test cases are part of the test suite like so:

> goldenTests :: IO TestTree
> goldenTests = do
>   fixtures <- GC.findByExtension [".md"] "tests"
>   tests <- mapM testFixture fixtures
>   return $ testGroup "Fixture files" tests
>     where testFixture n = do
>             fixture <- readFile n
>             let goldenF = stripped n ++ ".html"
>             let actual = either fail return $ harky fixture
>             return $ GC.goldenVsStringDiff n df goldenF actual
>           stripped = reverse . drop 3 . reverse
>           df ref new = ["diff", "-u", ref, new]
>           

This command runs the tests:

> testMain :: IO ()
> testMain = do
>   gT <- goldenTests
>   defaultMain $ testGroup "Harky"
>     [ tgItem
>     , tgAlternative
>     , tgSomeMany
>     , tgSatisfy
>     , tgSomeTill
>     , tgOneOf
>     , tgChar
>     , tgString
>     , tgLookAhead
>     , tgRunP
>     , tgChunks
>     , tgShowHTMLSnippets
>     , tgShowHTMLChunks
>     , gT
>     ]


Additionally, we made use of some helpers above. As they do not need
introductory prosa, we just put them down here:

> testDoesNotParse :: String -> Parser a -> String -> TestTree
> testDoesNotParse name p c = testCase name assertion
>   where assertion = isLeft (runParser p c) @? "Unexpected success"

> testParsesAs :: (Eq a, Show a) => String -> Parser a -> String -> a -> TestTree
> testParsesAs name p c expected = testCase name assertion
>   where assertion = runParser p c @?= Right expected


[1]: https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter3/parsec.hs
