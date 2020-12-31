""" An s-expression reader. Case preserving, with support
for both Common Lisp and Scheme style keywords.

#t  -> True
t   -> True
#f  -> False
nil -> tuple()

Not entirely sure what to do about None since there
is no direct equivalent.

Racket's (void) doesn't make sense because it is for
side effecting procedures (mostly), but its behavior
under eq? is consistent with None. (undefined) from
Racket also should not be used becuase it is an
implementation detail.

(equal (values) (values)) works in CL but
(equal? (values) (values)) does not work in Scheme.

'(), nil, #nil, or null all have different behavior
across Common Lisp, Emacs Lisp, Scheme, and Racket

The only thing that is portable across all of them
is the empty list '() or simply, the absense of a
value entirely. However, if we are using alists for
key value pairs, then this problem is sort of solved
for us, because '(key) <-> '(key . ()) in all cases
(though note the divergent behavior of '(key . null)
in Racket where only `(key . ,null) works).

Alternately we could partially avoid this by requiring
auth variables to be defined as keywords using :keyword
style, which racket can handle without too many issues.
Note that scheme has no standard for keyword arguments.
Clojure and Hy use :keyword syntax as well. So Schemes
seem to be the odd one out and will require additional
code to handle :keywords.

In terms of parens. () and [] parse as in their native
implementation's read. This is ok since the Common Lisp
impl to enable [] is a few lines for a reader macro.
Imples should not assume list, vector, etc. based on
the shape of the parens.

{} should be avoided since it has too many interpretations.
Given the small size of these configs, plists are the
preferred format and conversion into language specific
forms is up to the implementation. The parser supports
curly braces, but they are expected to be used like plists
or clojure/hy maps.

Ideally all of #; #+() and #_ would be supported and
should be converted to their respective version.
Unfortunately this means we can't use the elisp reader
directly. Both Hy and clojure support #_ racket is #;
and apparently so is r6rs and cl is #+() and friends.

Feature expressions are quite a fraught issue. Some
would say that they require a stateful reader, but I
do no think that this is the case because it is possible
to resolve inclusion/esclusion in a second pass. Yes this
does add some memory overhead to retain the potentially
commented expressions, but it does not lead to cases where
the reading of some future symbol would be changed.
Clojure has reader conditionals, but they are inside out
and work like cond which seems ... not composable.

Sigh. Feature expressions are _not_ easy to implement.
In CL they do the right thing and will skip over any
enabled feature expression until they reach another
feature expression so that they do not induce cascading
insanity if there are multiple feature expressions before
a statement, essentially _any_ negative feature expression
results in all following feature expressions being ignored.
We are not implementing this right now.

Note that while CL doesn't have a bytestring literal. That
is because bytestrings are literally just byte arras so
the usual array literal syntax works #() you just have to
use numbers which in a sense protects from misinterpretation
based on how things print. You then use ~string-to-octets~
and ~octets-to-string~ to interconvert. There is also coerce
which can be used to flip between string and list.

Quasiquote. So nearly everyone is sane, except for hy, which
has to content with the fact that python has a whole bunch of
deep semantics around comma, so it uses tilde as unquote. Ah
I see. Clojure uses ~ for unquote as well because they want
comma to be whitespace. This actually seems reasonable. It
shouldn't be too much work to add ~ as unquote for racket and
cl readers, elisp probably harder.

It is not at all clear to me that quasiquote is needed, however
it is not entirely clear to me how to allow efficient expression
of quoted symbols while also allowing a rudamentary form of let
or let* to be used.

Does quote break a symbol? Everyone: yes. Clojure and guile, nah.
Different schemes seem to have different rules for this !?!!? WAT.

The general principle is to implement only exactly as much in the
parser as is needed to ensure that expressions that are well formed
in their source dialect do not cause syntax errors. This means that
a few more complex features have been implemented in this reader
than necessarily need to be used in any particular context.
"""

# ah uh wow, I'm ... kind of impressed by how fast this thing is
# it was reading racket code fast enough that I didn't think it
# was actually doing it, also wow pypy3 blazing through this

# you could probably speed this up if each one letter string that
# we encountered had a stable id, then you could do nearly all
# the matching we need using only object identities

from types import GeneratorType
from unicodedata import name as uniname, lookup as unilookup


class Ast:

    def __repr__(self):
        if hasattr(self, 'value') and type(self.value) != type(self.__repr__):
            return f'<{self.__class__.__name__[:2]} {self.value}>'
        if hasattr(self, '_value'):
            return f'<{self.__class__.__name__[:2]} {self._value}>'
        elif hasattr(self, 'collect'):
            return f'<{self.__class__.__name__[:3]} {self.collect!r}>'
        else:
            raise Exception('oops')

    def __hash__(self):
        return hash((self.__class__, self.value))


class _LBase(Ast):

    @classmethod
    def from_elements(cls, *elements):
        # FIXME stupidly inefficient
        return cls(list(elements))

    def __init__(self, collect):
        self.collect = collect

    def __repr__(self):
        return f'<{self._o} {repr(self.collect)[1:-1]} {self._c}>'

    def __eq__(self, other):
        # FIXME yeah ... interned vs uninterned
        return type(self) == type(other) and self.collect == other.collect


class ListP(_LBase):
    _o, _c = '()'


class ListS(_LBase):
    _o, _c = '[]'


class ListC(_LBase):
    _o, _c = '{}'


class Keyword(Ast):
    # FIXME pretty sure we should get rid of these in the read pass
    # and let the individual implementations determine? but trickly
    # the tradeoff is that we have to make sure that we can
    # distinguish :|a b| and |:a b| internally if we read both
    # of them into Atom

    def __init__(self, collect):
        self._collect = collect
        self.value = ''.join(collect)

    def __eq__(self, other):
        return self.value == other.value

    def __hash__(self):
        # FIXME need clarity on what identity model we follow here wrt
        # where Keyword is defined due to python issues with __main__
        # vs module imports keywords are self interning or somthing
        # like that so they are not as big an issue as symbols/atoms
        return hash((self.__class__, self.value))


class EKeyword(Keyword):

    def __init__(self, collect):
        self.collect = collect

    def __eq__(self, other):
        # FIXME yeah ... interned vs uninterned
        return type(self) == type(other) and self.collect == other.collect

    def value(self, dialect_ak_escape):
        return dialect_ak_escape(collect)

    #def __repr__(self):
        #return f'<{self.__class__.__name__[:3]} {self.collect!r}>'

    def __eq__(self, other):
        return type(self) == type(other) and self.collect == other.collect



class Atom(Ast):
    """ Numbers, identifiers, booleans, and more.
        These are left uninterpreted. """

    def __init__(self, collect):
        self._collect = collect
        self.value = ''.join(collect)
        # TODO convert the atoms to python types
        '#t'
        '#f'
        't'
        'nil'
        '1234'

    def __eq__(self, other):
        # FIXME yeah ... interned vs uninterned
        return type(self) == type(other) and self.value == other.value

    def __hash__(self):
        # FIXME need clarity on what identity model we follow here wrt
        # where Atom is defined due to python issues with __main__ vs
        # module imports and lisp interned vs uninterned

        # XXX NOTE this only compares the Ast values !!!!
        # it does not deal with the fact that there needs
        # to be a second pass where atoms are processed
        # further into numbers, symbols, etc. we might even
        # consider moving keywords to this phase as well
        # XXX NOTE this also means that symbol names have
        # NOT BEEN RESOLVED AT THIS POINT!
        return hash((self.__class__, self.value))


class EAtom(Atom):
    # TODO FIXME figure out whether EThings a really subClassOf Things
    # or whether that will cause issues, in a sense, yes they are Chars
    # but they have different behavior and requirements

    def __init__(self, collect):
        self.collect = collect

    def __eq__(self, other):
        # FIXME yeah ... interned vs uninterned
        return type(self) == type(other) and self.collect == other.collect

    def value(self, dialect_ak_escape):
        return dialect_ak_escape(collect)

    #def __repr__(self):
        #return f'<{self.__class__.__name__[:3]} {self.collect!r}>'

    def __eq__(self, other):
        return type(self) == type(other) and self.collect == other.collect


class _EscBase(Ast):

    def __init__(self, value):
        #self._collect = collect
        #collect[0]
        self._value = value

    def __repr__(self):
        return f'<{self.__class__.__name__[:3]} {self._value!r}>'

    def __hash__(self):
        return hash((self.__class__, self._value))

    def __eq__(self, other):
        return type(self) == type(other) and self._value == other._value


class Escape(_EscBase):
    """ Symbol escapes """

    @property
    def value(self):
        # XXX NOTE THE DIFFERENCE FROM SEscape
        # symbol escapes do not have special
        # meaning, even in elisp, they just
        # prevent any special behavior from occuring
        # e.g. the end of a symbol or the start of a
        # number
        return self._value


class SEscape(_EscBase):
    """ String and char escape. """

    # TODO determine whether we need a separate
    # case for symbol escapes, I suspect that we
    # do not becuase they only function on a tiny
    # subset of chars and are invertable without
    # any ambiguity
    # XXX we do

    # FIXME escape needs to be its own Ast node

    # everyone deals with escapes differently
    # unfortunately escapes are not something
    # that most internal representations want
    # to keep around because they taint everything
    # this means that it isn't really possible
    # to keep the escapes around in the ast

    # as it turns out this is ok, we already
    # see that this happens with chars for elisp
    # so much better to reduce the level of
    # configuration needed prior to reading

    # this way we can move all the char and
    # string nonsense out of this file

    # the tradeoff of doing this is that
    # strings and chars will have two
    # different types, those where no
    # special syntax was used, and those
    # where some escape code was used
    # this seems reasonable to me since
    # deferring the determination of the
    # exact value until a later step has
    # a number of advantages and no obvious
    # drawbacks (yet)

    def value(self, dialect_escapes):
        return dialect_escapes(self._value)


class String(Ast):
    """ This is an unprocessed string that is still
    made out of its various parts. If you encounter
    an ast node of this type it should contain escaped
    charachters that need to be processed. """

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_escapes):
        ''.join([c.value(dialect_escapes)
                 if isinstance(c, SEscape)
                 else c for c in self.collect])

    #def __repr__(self):
        #return f'<{self.__class__.__name__[:3]} {self.collect!r}>'

    def __eq__(self, other):
        return type(self) == type(other) and self.collect == other.collect


class Char(Ast):

    known_multi = {
        # from Racket, this is how racket manages to implement the
        # char lit reader, they have a closed set of valid multichar
        # charachters whereas cl has an arbitrary number because
        # syntactically it allows any symbol name and then fails if a
        # char by that name is not know to the implementation
        # except of course for all the \u unicode escapes
        # NOTE elisp is the only one that supports ?\x all the rest
        # only support \u or #\u, except hy, which is py which has no chars
        'backspace': '\x08',  # aka '\b'
        'newline': '\x0a',    # ala '\n'
        'linefeed': '\x0a',   # ala '\n'
        'nul': '\x00',
        'null': '\x00',       # "\u0000" "\x00"
        'page': '\x0c',       # \f
        'return': '\x0d',     # \r
        'rubout': '\x7f',     # aka delete
        'space': '\x20',
        'tab': '\x09',        # \t
        'vtab': '\x0b',       # \v
    }

    def __init__(self, collect):
        self._value = ''.join(collect)
        return  # deferred until a later pass when the implementation details for chars are known

        #assert len(collect) == 1 or , collect
        if len(collect) == 1:
            self.value = collect[0]
        elif not collect:
            raise ValueError('Empty char??')
        else:
            print('charco:', collect)
            # yes, let's just add the need to handle higher order
            # logics and errors when using what appears to be a simple
            # predicate ... LOL PYTHON

            #if (isinstance(collect[0], type) and issubclass(collect[0], KeyChar) or
                #isinstance(collect[0], Octal)):
            if isinstance(collect[0], SEscape):
                # EEEEEMMMMMAAAAAACCCCCSSSSSS
                self.value = ELispCharThing(collect)
                return

            if v in self.known_multi:  # FIXME sbcl unicode #\LONG_NAME_THINGS
                self.value = self.known_multi[v]
            elif v.lower() in self.known_multi:  # FIXME and downcasing reader thing
                self.value = self.known_multi[v.lower()]
            elif v.startswith('u') or v.startswith('U') or v.startswith('x'):
                # FIXME the racket reader works differently for x
                self.value = chr(int(v[1:], base=16))
            elif v.startswith('U+'):  # SBCL
                self.value = chr(int(v[2:], base=16))
            #elif v.startswith('M-') or v.startswith('C-'):
                # FIXME TODO XXX these require extensions to the reader
                # to distinguish cases like M-C and M-\C-A
                #self.value = v
            elif v[0] in '0123':
                pass
                # XXX racket only runs from 000 to 377 somehow ??
                # ah yes, that is 255 in octal, so only the first byte
                # then you have to use unicode
            else:
                maybe_uniname = v.replace('_', ' ')
                try:
                    self.value = unilookup(maybe_uniname)
                    return
                except KeyError:
                    pass

                raise ValueError(f'Unknown charachter name {v!r}')

    #def __repr__(self):
        #return f'<{self.__class__.__name__[:2]} {self._value!r}>'

    def __eq__(self, other):
        return type(self) == type(other) and self._value == other._value

    def value(self, dialect_char):
        return dialect_char(self._value)


class EChar(Char):
    """ TODO chars with escape sequences in them """

    def __init__(self, collect):
        self.collect = collect

    #def __repr__(self):
        #return f'<{self.__class__.__name__[:2]} {self.collect!r}>'

    def __eq__(self, other):
        return type(self) == type(other) and self.collect == other.collect

    def value(self, dialect_char, dialect_escapes):
        value = ''.join([c.value(dialect_escapes)
                         if isinstance(c, SEscape)
                         else c for c in self.collect])

        return dialect_char(value)


class ELispCharThing(Ast):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f'<ELCT {self.value!r}>'

    def __hash__(self):
        return hash((self.__class__, self.value))

    def __eq__(self, other):
        return type(self) == type(other) and self.value == other.value


class Comment(Ast):
    """ Line comment """

    def __init__(self, collect):
        self.collect = collect
        self.value = ''.join(collect)


class BComment(Ast):
    """ Block comment """

    def __init__(self, collect):
        self.collect = collect
        self.value = ''.join([v._nest if isinstance(v, self.__class__) else v for v in collect])

    @property
    def _nest(self):
        return f'#|{self.value}|#'


class WrapsNext(Ast):
    @classmethod
    def from_collect(cls, collect):
        comments = [c for c in collect if isinstance(c, Comment) or isinstance(c, XComment)]
        collect = [c for c in collect if not (isinstance(c, Comment) or isinstance(c, XComment))]
        if not collect:
            # FIXME not all wraps next things require content to follow ?
            raise SyntaxError(f'Empty {cls.__name__}!')  # FIXME context for debug
        assert len(collect) == 1, collect
        self = cls(collect[0])
        if comments:
            self.comments = comments

        return self

    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        return self.value == other.value


class Quote(WrapsNext):
    """ HI there. """


class IQuote(WrapsNext):
    """ Quasiquote """


class UQuote(WrapsNext):
    """ Unquote """

    def __hash__(self):
        return hash((self.__class__, self.value))


class SUQuote(WrapsNext):
    """ Splicing Unquote """

    def __hash__(self):
        return hash((self.__class__, self.value))

    #def __repr__(self):
        #return f'<{self.__class__.__name__[:2]} {self.value!r}>'


class Sharp(WrapsNext):

    def ___init__(self, value):
        self._value = value

    @property
    def __value(self):
        # allow #t and #f
        try:
            if self._value.value == 't':
                return True
            elif self._value.value == 'f':
                return False
        except AttributeError as e:
            pass

        # basically I need a bit more thought about how to handle this
        # we may be able to add a set of well defined behaviors for a
        # reader for orthauth, but for now I'm going to limit it. If
        # this reader is used in another context, then we can just over
        # ride this error
        raise NotImplementedError('The meaning of # is too variable for routine use right now.')


class XComment(WrapsNext):
    """ eXpression comments, aka datum comments """


class FeatureExpr(WrapsNext):

    def ___init__(self, value):
        self._value = value

    @property
    def __value(self):
        raise NotImplementedError('Best not to use these right now.')


class State:
    state_names = {}  # update this with all the dialect variants of the names
    def __init__(self, index):
        self.index = index

    def __repr__(self):
        try:
            return f'<state {self.state_names[self]}>'
        except KeyError as e:
            breakpoint()
            pass


unp = object()  # unparsable  # cannot use None

escape_hat = SEscape('^')


class KeyChar:
    """ Base for char escape things. """


class UnicodeName(KeyChar):
    """ Elisp madness. """


class MetaNeedDash(KeyChar):
    """ Alt to the rest of us. """


class HyperNeedDash(KeyChar):
    """ ??? to the rest of us. """


class EhNeedDash(KeyChar):
    """ ??? to the rest of us. """


class ControlHat(KeyChar):
    """ Maybe this is cmd on macs? No idea. """


class ControlNeedDash(KeyChar):
    """ Maybe this is cmd on macs? No idea. """


class ShiftNeedDash(KeyChar):
    """ ?\S- ?? No idea. """


class SpaceOrSuperNeedDash(KeyChar):
    """ ?\s- ?? No idea. """


class Octal(KeyChar):
    """ ?\000 to ?\777 """

    def __init__(self, integer):
        self.value = integer

    def __repr__(self):
        return f'Octal({self.value})'


# #{ requires och = '{'
syms_base = ('(', ')', '[', ']',
             ';', '"', "'", '`',
             ',', '@',
             '\n', '\t', ' ', ':', '\\', '\\', unp,
             # supporting both ? and #\ for char lits beyond mad
             # unless they have the same behavior or ? must be used
             # with an explicit escape at all times ?\ the second
             # option seems reasonable

             # no, we are going to straight up not allow charachter
             # literals AT ALL in the sxpyr format, configuration
             # and interchange rarely need ... SIGH ... but they might
             '{', '}',
             '#', '|', '|')

syms_cl = ('(', ')', unp, unp,
           ';', '"', "'", '`',
           ',', '@',
           '\n', '\t', ' ', ':', '\\', '\\', unp,
           unp, unp,
           '#', '|', '|')

syms_el = ('(', ')', '[', ']',
           ';', '"', "'", '`',
           ',', '@',
           '\n', '\t', ' ', ':', '\\', '\\', '?',
           unp, unp,  # FIXME uses curlies but in a different context (which doesn't need pairing)
           '#', unp, unp)

syms_hy = ('(', ')', '[', ']',
           ';', '"', "'", '`',
           '~', '@', # diff
           '\n', '\t', ' ', ':', unp, '\\', unp,
           '{', '}',
           '#', unp, unp)

syms_clj = ('(', ')', '[', ']',
            ';', '"', "'", '`',
            '~', '@', # diff
            '\n', '\t', ' ', ':', unp, '\\', '\\',  # clj uses a bare backslash for char
            '{', '}',
            '#', unp, unp)

syms_gui = ('(', ')', '[', ']',
            ';', '"', "'", '`',
            ',', '@',
            '\n', '\t', ' ', ':', '\\', '\\', unp,
            unp, unp,
            '#', '|', '|')


if True:  # make sure we didn't accidentally forget to update a dialect
    from itertools import zip_longest
    def oops(b, d): print(f'misalignment or a swap {b!r} != {d!r}')
    [ds if (bs == ds or ds == unp) else oops(bs, ds) for d in
    (syms_cl, syms_el, syms_hy, syms_clj) for bs, ds in zip_longest(syms_base, d, fillvalue='aaaaaaaaaaaaa')]


cee_base = {
    'n': '\n',
    't': '\t',
    'r': '\r',
    'v': '\v',
    'b': '\b',
    'a': '\a',
    'f': '\f',
}

cee_oct = {
    '0': Octal(0), # OH DEAR, these read as octal
    '1': Octal(1),
    '2': Octal(2),
    '3': Octal(3),
    '4': Octal(4),
    '5': Octal(5),
    '6': Octal(6),
    '7': Octal(7),
}

cee_el = {
    # OH NO
    # given its origins in Emacs, elisp has has by far the
    # largest number of ways to represent the same charachter
    # not all of these are implemented by this reader since
    # even reading chars at all induces way more complexity
    # than I originally wanted
    **cee_base,
    **cee_oct,
    'd': '\x7f',  # delete aka rubout
    'e': '\x1b',  # escape
    's': SpaceOrSuperNeedDash,  # space
    #'s-': ????,  # FIXME AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    'C': ControlNeedDash,  # FIXME must be followed by a dash
    'M': MetaNeedDash,     # and emacs denotes this with a -1
    'S': ShiftNeedDash,
    'H': HyperNeedDash,
    'A': EhNeedDash,
    'N': UnicodeName,  # must be followed by {...}
    '^': ControlHat,  # emacs denotes this with a -1
}

cee_gui = {
    **cee_base,
    **cee_oct,
}


def configure(_symbols=None,
              quote_in_symbol=False,  # True, False, SyntaxError
              additional_whitespace=tuple(),
              #escape_always=True,
              #curlies_map=False,
              #char_esc_expand=tuple(),  # expand rules must be provided at runtime FIXME dict or function?
              char_auto_escape=True,  # FIXME the variety of behaviors has 2 axes I think, not clear what the default should be yet
              char_auto_end=True,  # FIXME same issue  XXX not sure we need this, somehow I think racket special cased these?
              #char_name_symbol=False,  # this is what cl does
              #immutable_cons=False,  # FIXME this should be deferred
              ):
    if _symbols is None:
        _symbols = syms_base
    #if not char_esc_expand:
        #char_esc_expand = cee_base

    eof = object()
    syms = [op, cp, os, cs, sc,
            dq, sq, gr, cm, at,
            nl, ta, sp, co, bs, bss, qm,
            oc, cc,
            ha, pih, pi,
            ] = [_ for _ in _symbols]

    # even though additional whitespace doesn't get an explicit name
    # it still needs to be in syms to prevent it from being included
    # in identifier symbols
    syms.extend(additional_whitespace)

    # other symbols that we may want to support directly in the reader
    am = '&'  # maybe want for lambda list control words?
    #qm = '?'
    #at = '@'  # TODO splicing unquote

    whitespace = (nl, ta, sp) + additional_whitespace
    scoe_complex = (nl, dq, bs, ha, pih)
    # note that pipe does not end atoms and keywords but the state still
    # transitions to v similarly exiting v does not exit atom or keyword
    ak_ends = (op, cp, os, cs, oc, cc, sc, dq, sq, gr, cm, *whitespace)
    '()[]{};"\'`,\n\t '
    if quote_in_symbol:
        ak_ends = tuple(s for s in ak_ends if s != sq)

    m_ends = (*ak_ends, qm, pi, ha)  # FIXME tricky because we need at least one char for m

    # XXX NOTE state could be defined globally but I think
    # keeping it in here will have better performance
    states = [bos, a, k, q, u, u1, us, i, s, t, l, d, h, c, e, es, x, f, blk, r, v, nestblk, m, m1] = [
        State(i) for i in range(24)]
    esclikes = e, m
    quotelikes = q, u, u1, us, i, x, f, h
    listlikes = t, l, d, blk

    # historical names
    #n = nestblk
    #o = blk  # as in blOck

    state_names = {
        bos: 'bos',
        a: 'a',
        k: 'k',
        q: 'q',
        u: 'u',
        u1: 'u1',
        us: 'us',
        i: 'i',
        s: 's',
        t: 't',
        l: 'l',
        d: 'd',
        h: 'h',
        c: 'c',
        e: 'e',
        es: 'es',
        x: 'x',
        f: 'f',
        blk: 'blk',
        r: 'r',
        v: 'v',
        nestblk: 'nestblk',
        m: 'm',
        m1: 'm1',
    }

    State.state_names.update(state_names)
    state_names = None
    del state_names


    def plist_to_dict(plist):
        # FIXME need a way to sanely control curlies for Racket vs clj/hy
        if len(plist) % 2 != 0:
            raise SyntaxError('plist is not a plist')
        try:
            return dict(zip(plist[::2], plist[1::2]))
        except TypeError as e:
            breakpoint()
            pass


    #if curlies_map:
        # FIXME hy #{} issue
        #collect_curlies = plist_to_dict
    #else:
        #collect_curlies = lambda l: l


    #caste_list = tuple if immutable_cons else (lambda l: l)
    #caste_vector = tuple

    def caste_string(collect):
        try:
            return ''.join(collect)
        except TypeError:
            return String(collect)

    def caste_atom(collect):
        return (EAtom(collect) if
                [_ for _ in collect if isinstance(_, Escape)] else
                Atom(collect))

    def caste_keyword(collect):
        return (EKeyword(collect) if
                [_ for _ in collect if isinstance(_, Escape)] else
                Keyword(collect))

    def caste(state, collect):
        # FIXME this second set of branches shouldn't be necessary?
        if   state == s: val = caste_string(collect)
        elif state == a: val = caste_atom(collect)
        elif state == c: val = Comment(collect)
        elif state == blk: val = BComment(collect)
        #elif state == r: val = BComment(collect)
        elif state == k: val = caste_keyword(collect)
        elif state == t: val = ListP(collect)
        elif state == l: val = ListS(collect)
        elif state == d: val = ListC(collect) # collect_curlies(collect)
        elif state == q: val = Quote.from_collect(collect)
        elif state == i: val = IQuote.from_collect(collect)
        elif state == u: val = UQuote.from_collect(collect)
        elif state == us: val = SUQuote.from_collect(collect)
        elif state == h: val = Sharp.from_collect(collect)  # TODO override
        elif state == x: val = XComment.from_collect(collect)
        elif state == f: val = FeatureExpr.from_collect(collect)  # TODO override
        elif state == v: val = caste_atom(collect)  # cl vs rr different behavior '|a \ b| '|a  b|
        #elif state == es: val = SEscape(collect)  # TODO
        elif state in (m1, m): val = (EChar(collect)
                                      if [_ for _ in collect if isinstance(_, SEscape)] else
                                      Char(collect))
        #elif state == bos: val = eof  # the only time we hit this should be eof
        else: raise NotImplementedError(f'Unknown state: {state} {collect}')
        return val


    def process_collect(collect_stack, state):
        return caste(state, collect_stack.pop())

        thing = collect_stack.pop()
        if collect is not None:
            assert collect == thing, f'\n{collect}\n!=\n{thing}'
            thing = caste(state, collect)
        else:
            thing = caste(state, thing)  # nested case

        return thing


    def push_state_from_char(collect_stack, collect, stack, char):
        """ transition forward """
        # worth function call overhead for readability
        def to_state(next_state):
            stack.append(next_state)
            collect_stack.append([])
            return None

        def to_state_collect(next_state):
            stack.append(next_state)
            _collect = []
            collect_stack.append(_collect)
            return _collect

        if   char == dq: collect = to_state_collect(s)
        elif char == sc: collect = to_state_collect(c)
        elif char == op: collect = to_state(t)
        elif char == os: collect = to_state(l)
        elif char == oc: collect = to_state(d)
        elif char == sq: collect = to_state(q)
        elif char == gr: collect = to_state(i)
        elif char == cm: collect = to_state(u1)
        elif char == at:
            breakpoint()
            collect = to_state(us)  # FIXME only if we are in state u
        # XXX not clear what we want to do with comma just yet
        # FIXME also not clear that we want to allow , and ~ to act as unquote in
        # just any old situation, but need clarity on why traditionally it always
        # is allowed to expand that way
        # I think the anwer is that checking to see if you are in some deeply nested
        # quasiquote situation could be extremely expensive and require long range
        # interactions in the reader, which I think would be ok for just this one
        # case ...
        # it kind of pushes us toward a full stack machine though because I think
        # it might be equivalent to being able to inspect the whole stack
        elif char == pi: collect = to_state_collect(v)
        # XXX warning: this branch for pipe only works correctly if
        # collect is None
        elif char == ha:
            state = stack[-1]
            if state == blk:  # FIXME do we really want to do this here
                # it means that we didn't handle a case correctly in parse
                stack.append(nestblk)  # FIXME maybe nest blk
            else:
                collect = to_state(h)
        elif char == qm:
            collect = to_state_collect(m1)
        elif char == bss:
            collect = to_state_collect(es)
        else:
            raise Exception('This should never happen.')

        return collect


    def resolve_pops(collect_stack, collect, stack, char):
        state = stack[-1]
        *cut, = i_resolve_pops(collect_stack, stack, char)

        if state == m1:  # FIXME using this to prevent issues with strings sigh
            collect = None
        elif (char in (op, os, oc, sq, gr, cm) or
              state == m and char in (qm, ha) or  # and char_auto_end ??? chars may end chars ie not cl
            # NOTE ha is indeed in m_ends
            #char == sc and state != m1 or
            #char == sc and state != m1 or
            #char == qm or  # ?a?b
            char == dq and state != s):
            collect = push_state_from_char(collect_stack, collect, stack, char)
        elif stack[-1] == blk:
            collect = collect_stack[-1]
            # a bit different from other listlikes because it is its own collector
            # in addition to a nested container for other block quotes
        else:
            collect = None

        return (collect, *cut)


    def i_resolve_pops(collect_stack, stack, char):
        if debug:
            print(f'RS: {char} {stack}')
            print(f'CO: {collect_stack}')
            pass
        state_prev = stack.pop()  # pop ^
        if (state_prev in (r,)):  # transition only no collect
            return i_resolve_pops(collect_stack, stack, char)

        # FIXME #\;a causes isses for a bunch of readers which
        # this behavior avoids, but requires a special case
        #if state_prev == m and char == sc:

        cut = process_collect(collect_stack, state_prev)  # cut /
        state = stack[-1]
        ##print(f'resp: {state_prev} {state} {cut!r}')

        if state_prev in listlikes and char in (cp, cs, cc):
            char = None

        #if state_prev == m1 and char == ')':
            #breakpoint()

        if char == eof:  # sigh using a branch for something that provably only happens once
            if state_prev in (s, v, *listlikes):
                # TODO unterminated x starting at position y

                raise SyntaxError(f'Unterminated thing {state_prev}.')

            if (state != bos or #and state_prev in (c, m, a) or
                state_prev == m1):
                collect_stack[-1].append(cut)
                return i_resolve_pops(collect_stack, stack, char)

            return cut,

        elif not collect_stack:
            return cut,
        elif state_prev == c:
            if state in (*quotelikes, *listlikes):
                # note that h is in quotelikes, but in this
                # case it should never match because sharp
                # whitespace should always cause an error
                collect_stack[-1].append(cut)
                return tuple()
            else:
                return cut,
        elif state in quotelikes:
            # FIXME make sure features eat twice?
            collect_stack[-1].append(cut)
            if state_prev is x and state is x:
                # don't comment things that are already commented
                # comment the thing that comes after them
                # FIXME this gets very tricky with feature expressions
                return tuple()

            return i_resolve_pops(collect_stack, stack, char)
        elif char in (cp, cs, cc) and state_prev in (*quotelikes, a, k, m, m1):
            # multiple things end at the same time
            collect_stack[-1].append(cut)
            if state_prev != m1:
                return i_resolve_pops(collect_stack, stack, char)
            else:
                return tuple()
        elif state in listlikes:
            collect_stack[-1].append(cut)
            return tuple()
        else:
            return cut,


    def process_h(collect_stack, stack, char):
        """ return True if should continue """
        # TODO in theory if we pass the generator in here
        # then we can make it possible to implement dispatch macros
        # transparently ...

        # pushed h, not popped h
        if   char in whitespace:   raise SyntaxError(f'# ws not allowed.')
        elif char in (cp, cs, cc): raise SyntaxError(f'# {char} not allowed.')
        #elif char == ha:
            #pass
            #stack.append(a)
            #breakpoint()
            #raise SyntaxError('## not allowed.')
        elif char == pih:  # FIXME essentially things that we want to ignore right now
            stack.pop()
            stack.append(blk)
            # FIXME need to update collect
            #collect_stack.pop()
            return True
        #elif char == cm:  # FIXME unquote outside quasiquote
            #stack.pop()
            #stack.append(u)  # FIXME this is actually unquote syntax
        elif char == bs:
            stack.pop()
            stack.append(m1)  # FIXME hrm
            return True
            #return not char_auto_escape  # FIXME
        elif char in '_;':
            stack.pop()
            # do not need collect_stack
            # reuse the collect from h for x
            stack.append(x)
            return True
        elif char in '+-':
            stack.pop()
            # do not need collect_stack
            # reuse the collect from h for f
            stack.append(f)
            return True


    def parse(iter_over_len_1_strings):
        #states  # SIGH
        bp = False
        #print(iter_over_len_1_strings)
        if not isinstance(iter_over_len_1_strings, GeneratorType):
            iter_over_len_1_strings = (_ for _ in iter_over_len_1_strings)

        stack = [bos]
        collect_stack = []
        collect = None
        old_ps = point_start = None
        line = 1
        # FIXME also need a point stack at which point it is
        # likely better to switch to having a combined stack
        # for state, collect, and point
        try:
            for point, char in enumerate(iter_over_len_1_strings):
                state = stack[-1]

                #if char == '?' and state == m and collect[-1] == '-':
                    #breakpoint()
                    #pass
                #if state == r and char == '#':
                    #breakpoint()
                #if state == bos and char == '|':
                    #breakpoint()
                #if state == s:
                    #breakpoint()
                if debug:
                    print('cs:', char, stack)
                    pass

                if char == nl:
                    line += 1

                if state == u1:
                    # FIXME can we integrate this into the main switch?
                    if char == at:
                        stack.pop()
                        stack.append(us)
                        continue
                    else:
                        stack.pop()
                        stack.append(u)
                        state = u

                if state == h:
                    # make any transitions that are required,
                    # fail on failure cases, or proceed
                    # needs to come before escape due to #\
                    if process_h(collect_stack, stack, char):
                        if stack[-1] in (blk, m1):
                            collect = collect_stack[-1]  # FIXME h should be via push_state_from_char no?
                        continue

                # handle escape
                if state in (e, es):
                    # escape is only needed for a single char
                    stack.pop()
                    # FIXME TODO add explicit Ast node for this
                    # so we can differentiate between the raw forms?
                    # yes, this is better than the alternative which
                    # is to hardcode escape rules into the reader
                    if state == es:
                        #char = caste(state, char)  # TODO make homogenous
                        char = SEscape(char)
                        #breakpoint()
                    else:
                        char = Escape(char)
                elif qm == '?' and state in (m1, m) and char == bs:  # FIXME HACK for elisp
                    stack.append(es)  # NOTE elisp uses the same escape sequences for chars and strings
                    continue
                elif state not in (c, blk, nestblk, m1) and char == bs:  # implicit not in (c, o, m1)
                    # FIXME TODO need to split \ in string from the rest
                    if state in (s, m):
                        stack.append(es)
                    else:
                        stack.append(e)
                    #breakpoint()  # XXX m debug start point
                    # FIXME interaction between m and e
                    continue
                elif state == s and char == bss:
                    # FIXME we should be able to handle this homogenously
                    stack.append(es)
                    continue

                #if stack[-1] == m and collect is None:
                    # FIXME escape issue
                    #collect = []  #  FIXME HACK

                #if state == m and char == qm:
                    #breakpoint()
                if state == m1:
                    if char in syms and char in m_ends:
                        collect.append(char)
                    else:
                        # FIXME action at a distance
                        # this pops here to keep the scoe
                        # block a bit simpler
                        stack.pop()
                    #stack.pop()
                    #state = stack[-1]

                #if False:
                if state in (e, es) and stack[-1] == m1: # state == m or
                    # FIXME elisp ?\a etc have special meaning
                    # that we should not handle in the reader
                    # so I think it needs to be read into its
                    # ast type so that ?a and ?\a are distinct

                    # FIXME m and e have sligh issues here
                    # needs to be handled after e
                    # ideally we could fall through
                    # since it pushes and pops on the same char
                    # but that means we would have to do something
                    # to reorder everything
                    # FIXME sigh, so now we copy and paste all the stuff from
                    # our usual resolve pops section here

                    # you would THINK that elisp would be implemented in this way
                    # and you would be wrong, because \\n and \n are not actually
                    # the same thing
                    #if (state == es and char in char_esc_expand):
                        #char = char_esc_expand[char]

                    collect = collect_stack[-1]  # inherit from the sharp or question mark?
                    collect.append(char)

                    if True:
                        pass
                    elif (state == es and char in char_esc_expand):
                        # it would appear that this is how elisp is
                        # actually implemented, it does the escaping
                        # after it reads, so it doesn't end the char
                        # name if an escape char is used, it just
                        # keeps going because a literal m_end was not
                        # used, this is ...  probably not a bug? it is
                        # just a bit confusing
                        collect.append(char_esc_expand[char])
                    else:
                        collect.append(char)

                    if char not in syms or char not in m_ends:
                        stack.pop()
                        stack.append(m)

                    # FIXME vs this ...
                    #collect = []
                    #collect_stack.append(collect)
                    #collect.append(char)  # this should be collect_stack[-1]
                    # FIXME chaining multiple
                    #if state == es:
                        # I don't think we need char_auto_end when we use es for elisp ?\
                        #continue
                    #elif char_auto_end and char in m_ends:
                    if char_auto_end and char in m_ends:
                        *thing, = i_resolve_pops(collect_stack, stack, char)
                        collect = None
                        yield from thing
                        # FIXME not sure if correct
                        if thing:
                            old_ps = point_start  # for debug
                            point_start = None
                        if collect is not None:
                            point_start = point
                        continue
                    else:
                        continue   # FIXME ... not sure if this is right well it sorta works

                if state == nestblk:
                    stack.pop()
                    if char == pih:
                        # FIXME sigh
                        stack.append(blk)
                        collect = []
                        collect_stack.append(collect)
                    else:
                        collect.append(pih)

                elif state == blk and char == pih:
                    stack.append(r)

                # handle all the + cases except
                elif (                     char not in syms or
                    state in (a, k)    and char not in ak_ends and char != pi or  # TODO see if we actually need the state test here
                    state in (m1, m)  and char not in m_ends or
                                           char == co or
                    state != u1           and char == at or  # FIXME do we really need u1?
                    state == h            and char == ha or  # FIXME should probably work like an FeaureExpr?
                    state == v            and char != pi or
                    state == r            and char != ha or
                    state in (s, c, blk, e, es) and char not in scoe_complex or
                    state in (s, c,      e, es) and char in (ha, pih) or
                    state in (s,    blk, e, es) and char == nl or
                    state in (   c, blk, e, es) and char     in (dq, bs) or
                    (qm == '?' and
                     # FIXME EEEEMMMMMAAAAAAAAAAAAACCCCCCSSSSSS
                     state == m and
                     char in m_ends and
                     (collect[-1] == ControlHat or  # FIXME now SEscape
                      collect[-1] == escape_hat or
                      collect[-1] == '-' and
                     len(collect) > 1 and  # FIXME HACK to avoid index error should be a state
                     # implicitly checking if there is a KeyChar in collect
                      (isinstance(collect[-2], type) or  # FIXME now SEscape
                       # TODO see if we accidentally induce bugs by reading
                       # things that the elisp reader would not
                       isinstance(collect [-2], SEscape))))):
                    if collect is None:  # if collect is a list it is for
                        # another state safe to assign state here, also
                        # required to handle the case where the loop
                        # immediately terminates
                        state = k if char == co else a
                        stack.append(state)
                        collect = []
                        collect_stack.append(collect)
                        if point_start is None:
                            point_start = point

                    if state == r and char != pih:
                        stack.pop()
                        collect.append(pih)
                    #elif (state == es and char in char_esc_expand):
                        #char = char_esc_expand[char]
                    elif state == m1:
                        stack.append(m)  # FIXME lurking transition

                    collect.append(char)
                    # FIXME if we are in state m and get here everything breaks
                    # this is where a goto would be really easy

                elif ((state in quotelikes or state == bos)
                                                    and char in (cp, cs, cc) or
                    state == t                      and char in (    cs, cc) or
                    state == l                      and char in (cp,     cc) or
                    state == d                      and char in (cp, cs    )):
                    raise SyntaxError('No matching paren!')

                # majority of pops happen here
                elif (state in (a, k) and char in ak_ends or
                    state in (m1, m) and char in m_ends or
                    state == t and char == cp or
                    state == l and char == cs or
                    state == d and char == cc or
                    state == s and char == dq or
                    state == c and char == nl or
                    state == r and char == ha):
                    #if state == m: breakpoint()
                    #if state == r: breakpoint()
                    #if state == m and char_auto_end: # XXX FIXME bad bad bad complexity sneeking in like this
                        #if collect is None:
                            #collect = []
                            #collect_stack.append(collect)
                        #print('bbbbbbbbbbbbbb', char)
                        #collect.append(char)

                    collect, *thing = resolve_pops(collect_stack, collect, stack, char)
                    yield from thing
                    # FIXME not sure if correct
                    if thing:
                        old_ps = point_start  # for debug
                        point_start = None
                    if collect is not None:
                        point_start = point

                elif char == pi:  # FIXME bad implementation should not need this special case
                    # pi and ha are cases where we might or might not want/need to updated collect
                    # which is why they are sort of separate because we stopped passing collect
                    # through push_state_from_char
                    if state == v:
                        stack.pop()
                        state = stack[-1]
                        # TODO
                        #breakpoint()
                    else:
                        # yep, just as complex as expected :/
                        if state in listlikes or state in quotelikes or state in (a, k, bos):
                            if collect is None:
                                stack.append(a)  # : must come outside if you want a keyword
                                collect = push_state_from_char(collect_stack, collect, stack, char)
                            else:
                                stack.append(v)
                        if point_start is None:
                            point_start = point

                elif state == blk and char == ha:
                    push_state_from_char(collect_stack, collect, stack, char)

                elif (state in listlikes or
                      state in quotelikes or
                      state == bos):
                    if char in whitespace:
                        #if state == f:
                            #raise SyntaxError('unterminated feature expression')
                        continue

                    collect = push_state_from_char(collect_stack, collect, stack, char)
                    if point_start is None:
                        point_start = point
                else:
                    breakpoint()
                    raise Exception("I can't believe you've done this")

            if collect_stack:  # EOF case
                #breakpoint()
                collect, *thing = resolve_pops(collect_stack, collect, stack, eof)
                yield from thing
                if stack and stack != [bos]:
                    # handle this special case out here to avoid needed to
                    # check if we are at eof internally
                    raise Exception('NOT DONE YET')

        except Exception as err:
            if point_start is None:
                point_start = old_ps
            ps = point_start + 1 if point_start is not None else 0
            pe = point + 1 if point is not None else 0
            msg = f'error start {ps} end {pe} line {line}'
            raise err.__class__(msg) from err


    return parse


def dialect(_parse, dialect=None):
    # NOTE this is NOT remotely safe for threads nor for any async
    # stuff, it might be possible to make the parser thread safe by
    # moving the symbols to be instance variables that could the be
    # overwritten, but then for cpython you pay the instance variabel
    # lookup cost, and we are already using global variables in the
    # parser which are slower to look up than local variables if we
    # were to make them almost completely immutable by defining them
    # inside the parser ... actually ... wait a second ...

    # there is an extremely annoying
    # tradeoff in python if you don't want to pay the
    # performance cost
    def inner(thing):
        try:
            set_dialect(dialect)
            yield from _parse(thing)
        finally:
            restore()

    # dialect parse
    unmatchable = '12'
    global os, oc, cs, cc
    try:
        if dialect == 'cl':
            restore = os, oc, cs, cc
            syms.remove(os)
            syms.remove(oc)
            syms.remove(cs)
            syms.remove(cc)
            os = oc = cs = cc = unmatchable

        yield from parse(thing)
    finally:
        syms.extend(restore)
        (os, oc, cs, cc) = restore


debug = False
printexp = False
def test():
    import sys
    global debug
    debug = '-d' in sys.argv
    global printexp
    printexp = debug or '-e' in sys.argv

    def sprint(gen, match=None):
        res = []
        for parsed_expr in gen:
            if printexp:
                print('expr:', parsed_expr)

            res.append(parsed_expr)

        #print(res)
        if match is not None:
            assert res == match, (res, match)

        return res

    parse = configure()
    parse_cl = configure(syms_cl)
    parse_el = configure(syms_el)
    parse_gui = configure(syms_gui,
                          quote_in_symbol=True,)
    parse_clj = configure(syms_clj,
                          quote_in_symbol=True,
                          #curlies_map=True,
                          #immutable_cons=True,
                          additional_whitespace=(',',),)
    parse_hy = configure(syms_hy)

    # asdf

    sprint(parse("'#:|fd\\sA|"))

    sprint(parse_clj("({'({:really? 'yes}) :wat 'thing :data} '({:really? 'yes}))"))
    sprint(parse_clj("({[{:really? 'yes}] :wat 'thing :data} [{:really? 'yes}])"))

    # these should read but will error later
    # because they have no label
    sprint(parse("#="))
    sprint(parse("##"))
    sprint(parse("#0=(1 2 3 . #0#)"))
    #sprint(parse("('##)"))  # TODO

    sprint(parse("#:|a b|"))
    # atom vs keyword distinction
    sprint(parse(":|a b|"))
    sprint(parse("|:a b|"))

    sprint(parse(":|d / e|"))
    sprint(parse("|:f / g|"))

    sprint(parse('#| #\; |#'))

    sprint(parse('#\\" lol'), match=[Char('"'), Atom('lol')])
    sprint(parse('(#\\))'))
    sprint(parse(r'"."'))
    sprint(parse(r'#\0'))
    sprint(parse(r'#\.'))
    sprint(parse(r'#\space'))

    sprint(parse(r'"\""'))

    sprint(parse(r"(#\0)"))
    sprint(parse(r"#\0"))
    sprint(parse("\\#"))
    sprint(parse("'\\#"))

    # FIXME this means we have to preserve the escapes in atoms too!!!!
    sprint(parse('\1e+NaN')) # -> symbol
    sprint(parse('1e+NaN'))  # -> number

    # sharp things

    #sprint(parse("#"))
    sprint(parse("##HRM"))
    sprint(parse("#;1 2"))
    sprint(parse("#; 3 4"))
    sprint(parse("#; hello"), match=[XComment(Atom('hello'))])
    sprint(parse("#_ there"), match=[XComment(Atom('there'))])
    sprint(parse("#a"))
    sprint(parse("#hash"))
    sprint(parse("#\\z"))
    sprint(parse("#'a"))
    sprint(parse("#`a"))
    sprint(parse("#,a"), match=[Sharp(UQuote(Atom('a')))])
    sprint(parse("#,@a"), match=[Sharp(SUQuote(Atom('a')))])

    # splicing unquote
    sprint(parse(r"'(regular , @ oops)"))
    sprint(parse(r"'(regular , @unquote)"))
    sprint(parse(r"'(splicing ,@unquote)"))
    sprint(parse(r"'(splicing ,@ unquote2)"))

    sprint(parse_el(r"(?\^?)"))

    sprint(parse_el(r'?a?b?-?\C-\C-\C-??d'))

    # weirdness with parsing chinese chars ... only happens if we read from file ??!?!
    # Oh great "冖" is really "AZ"" in byte from, of course emacs has support to auto convert
    # which is the file we were just looking at, but python hasn't he faintest idea
    # coding:iso-2022-7bit is in the file header which is why this was happening
    sprint(parse_el('(defvar lol \'(("冖")))'))

    # the ?\C-' issue
    sprint(parse_el(r"[?\C-\']"))

    # the ?\C-` issue
    sprint(parse_el(r"[?\C-\`]"))

    # the ?\C-? issue
    #sprint(parse_el(r"[?\C-?]"))  # XXX there is a note in the manual about this
    # stating that it is used all over the place ;_; grep -r "[?][\\]C-[?]"
    # not incredibly bad, but still problematic? and yes, requires special cases
    sprint(parse_el(r"[?\C-\?]"))  # sed -i "s/\([?][\\]C-\)[?]/\1\\\?/g"

    sprint(parse_el("(?\\123)"))  # oh no, python has this too ... implicit octal escapes
    # actually maybe it is a bug in what I'm doing here? hrm don't think so
    sprint(parse_el("(kp-separator ?,)"))
    # and best of all, of course you have to escape space, you just don't
    # have to escape other things that also end symbols, I can feel the
    # cruft of bad early design that as simply lingered for far too long
    sprint(parse_el("(define-key map [?\\S-\\ ] 'scroll-down-command)"))

    # FIXME FIXME sigh ?\C-, issue
    #sprint(parse_el("(define-key map [?\C-,]  'icomplete-backward-completions)"))  # FIXME now _THIS_ requires more states in the reader or tweaks to how m1 behaves specifically for C- and M- because ?\C- is its own thing and automatically escapes or goes back to state m1

    sprint(parse_el(r"(define-key map [?\C-\,]  'icomplete-backward-completions)"))  # this is how we can read such things, but full elisp compat requires the additional states as expected ... ARGH

    sprint(parse_el(r"(define-key map [?\M-\C-m] 'mpc-tagbrowser-dir-toggle)"))
    sprint(parse_el("(?\\N{lol I am not a unicode charachter name})"))  # FIXME this reads but incorrectly
    # NOTE we aren't going to fix this though since it is possible to recover
    sprint(parse_el("(?\\N{sigh}oops)"))  # oh boy, yeah, this is an error
    # but those curlies don't work like braces, they are just part of the symbol name
    # ugh what a mess, definitely not reading this stuff right now

    # charachter literals
    def make_test_char(char_marker):
        def inner (s):
            if isinstance(s, str):
                return '(' + char_marker + s + ')'
            else:
                return '(' + ''.join([char_marker + ss for ss in s]) + ')'

        return inner

    test_suffixes = (
              ("\\", ListP.from_elements(Char('\\'),)),
              (" ", ListP.from_elements(Char(' '),)),
              (";", ListP.from_elements(Char(';'),)),
              (")", ListP.from_elements(Char(')'),)),
              ("z", ListP.from_elements(Char('z'),)),  # can't use a because emacs -> x07
              ("  a", ListP.from_elements(Char(' '), Atom('a'))),
              ("  'a", ListP.from_elements(Char(' '), Quote(Atom('a')))),
              (") a", ListP.from_elements(Char(')'), Atom('a'))),
              (" 'a", ListP.from_elements(Char(' '), Quote(Atom('a')))),
              (")'a", ListP.from_elements(Char(')'), Quote(Atom('a')))),
              (" a", ListP.from_elements(Char(' '), Atom('a'))),
              (";a", ListP.from_elements(Char(';'), Atom('a'))),
              (")a", ListP.from_elements(Char(')'), Atom('a'))),
              ("'a", ListP.from_elements(Char("'"), Atom('a'))),
              ("aa", ListP.from_elements(Char('aa'),)),  # XXX REMINDER that non-existent char errors are NO LONGER reader errors
              (("y", "z"), ListP.from_elements(Char('y'), Char('z'))),
              ("newline", ListP.from_elements(Char('newline'),)),
              )
    effs = dict(
        tcq = make_test_char('?'),
        tcqe = make_test_char('?\\'),
        tcs = make_test_char('#\\'),
        tcb = make_test_char('\\'),
    )
    for key, fun in effs.items():
        for (string, ast) in test_suffixes:
            # todo for parser in (parse, parse_cl) etc. based on which fun/key is used
            tstr = fun(string)
            if debug:
                print('tc:', repr(tstr))
            if key in ('tcq', 'tcqe'):
                parser = parse_el
            elif key == 'tcb':
                parser = parse_clj
            else:
                parser = parse

            #print('string:', string)
            if (#string == 'aa' and key != 'tcqe' or
                #string == 'newline' and key == 'tcqe' or
                string == "'a" and parser == parse_clj):# or string == 'a)' and key == '':
                sprint(parser(tstr))  # will fail in the second pass
                #try:
                    #sprint(parser(tstr), match=[ast])
                    #assert False, 'should have failed'
                #except ValueError as e:
                    #pass
            elif (string == '\\' and key == 'tcq'):
                try:
                    sprint(parser(tstr), match=[ast])
                    assert False, 'should have failed'
                except SyntaxError as e:
                    pass
            elif key == 'tcqe':
                if ast is not None:
                    ast = ast.from_elements(*(EChar([SEscape(_._value)])
                                              if isinstance(_, Char)
                                              else _ for _ in ast.collect))

                # a number of these now read without error and the
                # fact that there is no such charachter is deferred
                if string == ' a':
                    ast = ListP.from_elements(EChar([SEscape(' '), 'a']),)
                elif string == ';a':
                    ast = ListP.from_elements(EChar([SEscape(';'), 'a']),)
                elif string == ')a':
                    ast = ListP.from_elements(EChar([SEscape(')'), 'a']),)
                elif string == '\'a':
                    ast = ListP.from_elements(EChar([SEscape('\''), 'a']),)
                elif string == 'aa':
                    ast = ListP.from_elements(EChar([SEscape('a'), 'a']),)
                elif string == 'newline':
                    ast = ListP.from_elements(EChar([SEscape('n'), *'ewline']),)

                sprint(parser(tstr), match=[ast])
            else:
                sprint(parser(tstr), match=[ast])

    #tc_el_esc
    #tc_el_uesc
    #tc_cl
    #tc_clj
    #tc_rr
    #tc_ga
    #tc_gu

    # charlits old
    sprint(parse(r'(list #\ a)'))
    sprint(parse(r'(list #\,a)'))
    sprint(parse(r'(list #\`a)'))
    sprint(parse(r'(list #\|a)'))

    sprint(parse(r'(list ?\ a)'))
    sprint(parse(r'(list ?\,a)'))
    sprint(parse(r'(list ?\`a)'))
    sprint(parse(r'(list ?\|a)'))

    sprint(parse_clj(r'(list \a\b\c)'))
    sprint(parse_clj(r'(list \a\b\c)'))
    # FIXME ?asdf and \asdf both cause read errors

    sprint(parse('(modify-syntax-entry ?\{  "(}1nb" table)'))
    sprint(parse_el('(modify-syntax-entry ?\{  "(}1nb" table)'))
    sprint(parse_el("?a?b?c"))
    sprint(parse_el("(list ?a?b?c)"))
    sprint(parse_el("""?
"""))

    sprint(parse_el("? "))
    sprint(parse_el("?)"))
    # wow, so this actually works, it is just hard to get
    # it to evaluate must be a legacy thing
    sprint(parse_el("?("))

    sprint(parse("#|| lol |||||#"))
    sprint(parse("a #| b #| c |# d |# e"))

    sprint(parse("(|Append| '|Append|)"))
    sprint(parse_cl("(`())"))
    #sprint(parse_cl("(a (b (c)) (d (e) `(f (g) h) ((i) j)))"))
    #sprint(parse("(a (b (c)) (d (e) `(f (g)  (1 (2)) (3 (4)) (ca 'q (* (f (1) (2))h))) ((i) j)))"))

    sprint(parse("a|b c|d"))
    sprint(parse("|a|b "))
    sprint(parse("|a| b"))

    # welcome to the watverse
    sprint(parse("(and 'a'b'c)"))

    sprint(parse_clj(":see-alsos [:clojure.core/* :clojure.core/*'],"))  # clojure weirdness  TODO

    # O ... K ... today we learned something about python strings
    # which brings us back to the question of why our example was failing
    sprint(parse(r";\
    "))
    sprint(parse(r";\
I can't believe you've done this."))
    sprint(parse(
        # yeah ... you just bashed yourself you idiot
        # \ escaping a literal newline is a rookie mistake
        # in a fit of extreme annoyance, there must be a newline between
        # the parens right now, maxiumum WAT
        # actually this is even more loltastic than you might realize
        # what I have /also/ done is to escape the newline at the end
        # of a comment so the comment eats the open paren on the newline
        # which is why I need _another_ newline in between the two parens
        # in order to evoke the error because you need the second newline
        # so that the closing parent isnt commented out

        # in common lisp you can in fact escape a newline but NOT in a comment
        # this is also true in racket and in elisp and they all protect against
        # the escape in the comment

        # XXX actually this is the result of the fact that inside a python string
        # you can escape a newline with a \ ... sigh
r""";\
( 
)"""
#"""
))
    sprint(parse('(#\\))'))
    sprint(parse_cl(
        # common lisp reader making our day super bad for square brackets
    r"""
(with-test (:name (compile * :constant-behind-cast :lp-1717971]))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda (x)
         (declare (type integer x))
         (declare (optimize (space 3) (compilation-speed 0)))
         (catch 'ct5
           (* (flet ((%f (&key (x (throw 'ct5 123)))
                       (the integer x)))
                (%f))
              x)))
    ((45) 123)))
"""))
    sprint(parse('#\['))
    sprint(parse('#| | |#'))

    sprint(parse(r'"\\"'))
    sprint(parse(r'("\\")'))
    sprint(parse(r'[("\\")]'))

    sprint(parse("(define lol #f)"))
    sprint(parse("[define hah #t]"))

    sprint(parse("{:a 1 :b 2 :c 3}"))
    sprint(parse("{:a 1 :b {hello there} :c 3}"))

    sprint(parse("#'stx"))
    sprint(parse("#`stx"))

    # feature expressions
    sprint(parse("#+\n()"))
    sprint(parse("#-\n()"))
    sprint(parse("#- (not feature)"))

    sprint(parse("#+()"))
    sprint(parse("#-()"))
    sprint(parse("#-(and yes I have features)"))
    sprint(parse("#-nil"))
    sprint(parse("#+nil"))
    sprint(parse("#+nil #-nil #-nil #-nil 'a"))
    sprint(parse("#+(or oh no) woop"))

    # eXpression comments
    sprint(parse("#;\n(a #; b c)"))
    sprint(parse("(a #; b c)"))
    sprint(parse("(a #_ b c)"))
    sprint(parse("(a #_#; b c)"))
    sprint(parse("(a #; #; b c)"))
    sprint(parse("(a #_ #_ b c)"))

    sprint(parse("' ; shi\nt"))

    sprint(parse('(a"b\\"c"d)'), match=[
        ListP.from_elements(
            Atom('a'),
            String(['b', SEscape('"'), 'c']),
            Atom('d'),)
    ])  # should fail

    sprint(parse("('())"), match=[
        ListP.from_elements(Quote(ListP.from_elements()),)
    ])

    sprint(parse("('()) a"), match=[
        ListP.from_elements(Quote(ListP.from_elements()),),
        Atom('a')
    ])

    sprint(parse("(X\"\")"), match=[
        ListP.from_elements(
            Atom('X'),
            '')
    ])
    sprint(parse("'(1'q)"), match=[
        Quote(
            ListP.from_elements(
                Atom('1'),
                Quote(Atom('q'))))
    ])
    sprint(parse("'(('q)a)"), match=[
        Quote(
            ListP.from_elements(
                ListP.from_elements(
                    Quote(Atom('q')),),
                Atom('a')))
    ])

    sprint(parse("('(x))"))
    sprint(parse("()"))
    sprint(parse("(())"))
    sprint(parse("(()())"))
    sprint(parse("'(1)"))
    sprint(parse("'(1())"))
    sprint(parse("'(('q))"))
    sprint(parse("''''''''''q"))

    # quasiquote
    sprint(parse("(`(x))"))
    sprint(parse("`(1)"))
    sprint(parse("`(1())"))
    sprint(parse("`((`q))"))
    sprint(parse("``````````q"))
    sprint(parse("`(1(2)`(3)(`4)`(`5))"))

    sprint(parse("(a (b)) c"))
    sprint(parse("'(1(2)'(3)) "))
    sprint(parse("'(1(2)'(3))"))

    sprint(parse("'(1(2))"))
    sprint(parse("'(1(2)'3)"))
    sprint(parse("'(1(2)'(3)('4))"))
    sprint(parse("'(1(2)'(3)) oops"))
    sprint(parse("'(1(2)'(3)('4)f)"))
    sprint(parse("'(1(2)'(3)('4)())"))
    sprint(parse("'(1(2)'(3)('4)'())"))
    sprint(parse("'(1(2)'(3)('4)'('5))"))
    sprint(parse('(a (b))\nc'))
    sprint(parse('(d ; comment\n(e))\nf'))
    sprint(parse(';comment'))
    sprint(parse('(symbol :keyword 1234 "string")'))
    sprint(parse('symbol'))
    sprint(parse('1234'))
    sprint(parse('"string"'))
    sprint(parse('t'))
    sprint(parse('#t'))
    sprint(parse('#f'))
    sprint(parse("'()"))
    sprint(parse("'quoted-symbol"))
    sprint(parse("       leading whitespace"))
    sprint(parse("trailing whitespace               "))
    sprint(parse("   'leading"))
    sprint(parse("'trailing    "))
    sprint(parse("'(((((((((((())))))))))))"))
    sprint(parse("wat 'trailing"))
    sprint(parse("symbol'quote more"))
    sprint(parse(":keyword'quote"))
    sprint(parse("''quote"))
    sprint(parse("'quote'quote"))
    sprint(parse("'quote(sym1)"))
    sprint(parse("'quote[sym2]"))
    sprint(parse("sym(sym3)"))
    sprint(parse("sym[sym4]"))
    sprint(parse("1[sym5]"))
    sprint(parse("2[sym6]"))
    sprint(parse("(a (b))"))
    sprint(parse("'(a (b))"))
    sprint(parse("'(1 2 3 4 (asdf asdf))"))
    sprint(parse("'(5 6 7 8 (asdf asdf))\n(desire to know more intensifies)"))
    sprint(parse("(defun asdf (there) 'hello there)"))
    sprint(parse("I am multiple atoms that should be read in a row. (indeed) \"yes\""))
    sprint(parse('(testing "I am a happy string \\" yay!" string escape)'))
    # NOTE if you have unmatched parens in a here string that means that
    # this parser will fail
    sprint(parse("""
;;; hrm
#;
(
#<<-- oh boy
don't look now, a
here strings ... the absolute worst because
your reader is no longer stateless, unlikely
that these will be allowed, they induce quite
a bit for complexity
 oh boy
)
"""))

    test_cfg1 = '''(:config-search-paths
("user-1.yaml" "{:user-config-path}/program-name/config.yaml")
:auth-variables (
:var-whose-default-value-is-nil/null/None ; FIXME
:var-name-1 ""
)
)
'''
    sprint(parse(test_cfg1))
    #return

    from pathlib import Path
    git_path = Path('~/git/').expanduser()
    git_nofork_path = git_path / 'NOFORK'
    paths = ((git_path / 'protc/anno-tags.rkt'),
             #(git_nofork_path / 'sbcl/src/compiler/generic/early-objdef.lisp'),
             #(git_nofork_path / 'sbcl/src/code/print.lisp'),
             #Path('/usr/share/guile/2.2/ice-9/i18n.scm'),
             #(git_nofork_path / 'emacs/lisp/international/titdic-cnv.el'),
             #(git_nofork_path / 'emacs/lisp/isearch.el'),
             #(git_nofork_path / 'emacs/lisp/icomplete.el'),
             #(git_nofork_path / 'emacs/lisp/mpc.el'),
             #(git_nofork_path / 'emacs/test/src/syntax-tests.el'),
             #(git_nofork_path / 'clojure/test/clojure/test_clojure/errors.clj'),
             #(git_nofork_path / 'racket/racket/src/cs/schemified/schemify.scm'),  # broken due to |#%name|
             #(git_nofork_path / 'racket/racket/src/schemify/wrap.rkt'),
             #(git_nofork_path / 'sbcl/tests/data/wonky3.lisp'),
             #)
    #_sigh = (

             # edn
             Path('../test/export.edn'),

             # clj
             *(git_nofork_path / 'clojure').rglob('*.clj'),
             *(git_nofork_path / 'spec.alpha/').rglob('*.clj'),
             *(git_nofork_path / 'core.specs.alpha/').rglob('*.clj'),
             *(git_nofork_path / 'tawny-owl/').rglob('*.clj'),

             # common lisp
             *(git_nofork_path / 'sbcl/').rglob('*.lisp'),

             # hy
             *Path('/usr/lib/python3.7/site-packages/hy/contrib/').rglob('*.hy'),

             # chez
             *(git_nofork_path / 'racket/').rglob('*.scm'),

             # racket
             *(git_path / 'protc/protc-lib/protc/').rglob('*.rkt'),
             *(git_nofork_path / 'racket/').rglob('*.rkt'),  # wow ... fast

             # guile
             *Path('/usr/share/guile/2.2/').rglob('*.scm'),

             # gambit (wow that install location)
             *Path('/usr/lib64/').rglob('*.scm'),

             # elisp
             *(git_nofork_path / 'emacs/').rglob('*.el'),  # elisp uses ? for chars
             *(git_nofork_path / 'org-mode/').rglob('*.el'),  # elisp uses ? for chars
             # https://yoo2080.wordpress.com/2013/11/25/question-mark-and-emacs-lisp/

             )

    should_fail = (
        'compiler-output-test.lisp',
        'wonky3.lisp',

        # racket + racket chez layer
        #'interpret.rkt',   # FIXME not really should fail but |asdf asdf| not impl
        #'infer-name.rkt',  # |[| issue
        'mk-gdbinit.rkt',  # here string killing things
        'rktc-utils.rkt',  # at-exp declared on 3rd line, comment on previous lines
        'new.rkt',         # here string
        #'xform.rkt',       # |;| issue
        #'schemify.scm',    # |#%name| issue

        # guile
        # https://www.gnu.org/software/guile/manual/html_node/Symbol-Read-Syntax.html
        # section 6.6.6.6 appropriately enough, except no mention in guile about the 'a'b'c problem
        #'psyntax-pp.scm',  # #{}# guile has some weird #{ }# syntax thing ... what the heck is this?!
        #'psyntax.scm',     # #{}#
        #'i18n.scm',        # guile allows things like number' to be symbols ?!?!?!?!? apparently so
        #'parser.scm',      # #{}#
        #'compile-tree-il.scm',  # #{}# no idea what this is, but it is annoying, seems like the cl/rkt pipe?
        #'function-slot.scm',  # #{}# seems like it is being used to define elisp functions or something?
        #'autofrisk.scm',   # #{}#

        # elisp
        #'admin.el',        # ? issue
        #'unidata-gen.el',  # ?
        #'syntax-tests.el', # hah this is handy, and yeah the ? issue
        #'print-tests.el',  # elisp allows '## ?? what the heck is it even?
        #'json-tests.el',   # '##
        #'org-list.el',     # '##
        #'autoinsert.el',   # ?
        #'woman.el',        # ?
        #'minibuffer.el',   # ?
        #'completion.el',   # ?
        #'json.el',         # ?
        #'pcmpl-unix.el',   # ?
        #'skeleton.el',     # ?
        'titdic-cnv.el',    # coding:iso-2022-7bit kills this one

        #'simple.el',       # ?\C-? issue
        #'icomplete.el',    # ?\C-, issue
        #'ido.el',          # ?\C-, issue
        #'isearch.el',      # ?\^? issue
        #'iswitchb.el',     # ?\C-, issue
        #'iris-ansi.el',    # ?\C-' ?\C-` and ?\C-, issues
        #'ns-win.el',       # ok, I give up, elisp char syntax is too complex
        #'xterm.el',        # ?\C-, infuratingly the ?\C-\' on the line above it _was_ commented

        # clojure
        #'reducers.clj',    # *' issue
        #'transducers.clj', # end with ' issue ... need to look into this ... very rare to see but not banned
        #'memorise_test.clj',  # #{ issue
        #'owl.clj',         # end with '
    )
    fail = True
    for count, p in enumerate(paths):
        p = p.expanduser()
        with open(p, 'rt') as f:
            try:
                hrm = f.read()
            except UnicodeDecodeError as e:
                print('fail unicode:', p)
                continue

            if hrm.startswith('#lang at-exp'):
                # can't parse @exp
                continue

            try:
                res = sprint(parse(hrm))
                print('success:', p)
                #val = list(parse(hrm))
            except Exception as err:
                try:
                    if p.name in should_fail:
                        continue
                    elif p.suffix == '.lisp':
                        print('generic failed:', p, err)
                        res = sprint(parse_cl(hrm))
                        print('success cl:', p)
                        #breakpoint()
                        # TODO
                        continue
                    elif p.suffix == '.el':
                        print('generic failed:', p, err)
                        #try:
                        res = sprint(parse_el(hrm))
                        print('success el:', p)
                        #except SyntaxError as err2:
                            #if '?' in hrm:
                                #print('fail ? issue:', p, err2)
                            #else:
                                #raise err2
                        continue
                    elif p.suffix in ('.clj', '.edn'):
                        print('generic failed:', p, err)
                        res = sprint(parse_clj(hrm))
                        #if p.suffix == '.edn':
                            #breakpoint()
                        print('success clj:', p)
                        continue
                    elif p.suffix == '.scm' and 'guile' in p.as_posix():
                        print('generic failed:', p, err)
                        res = sprint(parse_gui(hrm))
                        print('success gui:', p)
                        continue
                except Exception as err2:
                    raise err2.__class__(p) from err2

                debug = True
                try:
                    sprint(parse(hrm))
                except:
                    pass

                debug = False

                # racket files that use |these types of ids|
                # or escape brackets |[| cause issues
                if fail:
                    raise SyntaxError(p) from err
                else:
                    print(err)
                    print('fail path:', p)

            #sprint(v)

    print('total paths:', count + 1)

    return
    bads = ("(defun fail (parse)",
            '"I am an unterminated string',
            'unmatched ) right paren',
            'unmatched ] right bracket',
            "{",
            "}",
            '"',
            "'",  # need a proper error
            "' ; oops",  # should fail but is not
            "[)",
            "(]",
            "'(1(2)')",  # confusing error message
            "#+ ()",
            "|a| b|",
            '#| #| |#',
            '(;hrm',  # comment state could mask unterminated list
            '(#;',
            '(#||#',
            '#',
            '# ',
            '##',
            '#;',
            '#_',
            '#\\',
            )

    should_have_failed = []
    for bad in bads:
        try:  # I WANT MACROS
            sprint(parse(bad))
            should_have_failed.append(bad)
        except SyntaxError as e:
            print(f'task failed succssfully for {bad!r}')

    assert not should_have_failed, should_have_failed


if __name__  == '__main__':
    test()
