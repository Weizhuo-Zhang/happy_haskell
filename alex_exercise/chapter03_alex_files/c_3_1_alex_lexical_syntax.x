-- Alex's lexical syntax is given below. It is written as a set of macro
-- deficitions using Alex's own syntax. These macros are used in the BNF
-- specification of the syntax later on.

$digit                  = [0-9]
$octdig                 = [0-7]
$hexdig                 = [0-9A-Fa-f]
$special                = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic                = $printable # $white

@string                 = \" ($graphic # \")* \"
@id                     = [A-Za-z][A-Za-z'_]*
@smac                   = '$' id
@rmac                   = '@' id
@char                   = ($graphic # $special) | @escape
@escape                 = '\\' ($printable || 'x' $hexdig+ | 'o'$octdig+ | $digit+)
@code                   = -- curly brances surrounding a Haskell code fragment



-- optional phrases are enclosed in square brackets ('[ ...]'),
-- and phrases which may be repeated zero or more times are enclosed in braces
-- ('{ ... }'). Literal text is enclosed in single quotes

-- A Alex lexical specification is normally placed in a file with 'a.x'
-- extension. Alex source files are encoded in UTF-8, just like Haskell sourfe
-- files.
--
-- The overall layout of an Alex file is:
--
--     alex := [ @code ] [ wrapper ] { macrodef } @id ':-' { rule } [ @code ]
--
-- The file begins and ends with optional code fragment. These code fragment are
-- copied verbatim into the generated source file.
--
-- At the top of the file, the code fragment is normally used to declare the module
-- name and some imports, and that is all it should do: don't declare any functions
-- or types in the top code fragment, because Alex may need to inject some imports
-- of its own into the generated lexer code, and it does this by adding them
-- directly ager this code fragment in the output file.
--
-- Next comes an optional wrapper specification:
--
--     wrapper := '%wrapper' @string
--


3.2.1 Macro definitions
Next, the lexer specification can contain a series of macro definitions. There
are two kinds of macros, 'character set macros' which begin with a '$', and
'regular expression macros' which begin with a '@'. A 'character set macro' can
be used wherever a character set is valid.

    macrodef "= @smac '=' set
              | @rmac '=' regexp


3.2.2 Rules
The rules are heralded by the sequence 'id :-' in the file. It doesn't matter
what you use for the identifer, it is just there for documentation purposes. In
fact, it can be omitted, but the :- must be left in.

The syntax of rules is as follows:

    rule    :=  [ startcodes ] token
             |  startcodes '{' { token } '}'

    token   :=  [ left_ctx ] regexp [ right_ctx ] rhs

    rhs     :=  @code | ';'

Each rule defines one token in the lexical specification. When the input stream
matches the regular expression in a rule, the Alex lexer will return the value
of the exprression. Alex only places one restriction on actions: all the actions
must have the same type. They can be values in a token type, for example, or
possibly operations in a monad.

The action may be missing , indicated by replacing it with ';', in which case
the token will be skipped in the input stream.

Alex will always find the longest match. For example, if we have a rule that
matches whitespace:

    $white+         ;

Then this rule will match as much whitespace at the beginnning of the input
stream as it can. Be careful: if we had indtead written this rule as

    $white*         ;

then it would also match the empty string, which would mean that Alex could
never fail to match a rule!

When the input stream matches more than one rule, the rule which matches the
longest prefic of the input stream wins. If there are still several rules which
match a equal number of characters, then the rule which appears earliest in the
file wins.

3.2.2.1 Contexts
Alex allows a left and right context to be placed on any rule:

    left_ctx    := '^'
                 | set '^'
    right_ctx   := '$'
                 | '/' regexp
                 | '/' @code

The left context matches the character which immediately precedes the token in
the input stream. The character immediately preceding the beginning of the
stream is assumed to be '\n'. The special left-context '^' is shorthand for
'\n^'.

Right context is rather more general. There are three forms:

/ regexp
    The right-context causes the rule to match if and only if it is followed in
    the input stream by text which matches 'regexp'.

    NOTE: this should be used sparingly, because it can have a serious impact on
    performance. Any time this rule could match, its right-context will be
    checked against the current input stream.
