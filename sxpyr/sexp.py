__version__ = '0.0.1'

from ast import literal_eval
from json import dumps
from types import GeneratorType

debug = False


def make_do_path(do, chunksize=4096):
    """ along the way to load """
    #parse = configure(**kwargs)  # FIXME pass in parse
    #read = conf_read(parse, mawp)  # FIXME
    def do_path(path):
        def path_gen():
            with open(path, 'rt') as f:
                while True:
                    data = f.read(chunksize)
                    if not data:
                        break
                    yield from data

        return do(path_gen())

    return do_path


def conf_read(parser, walk_help):
    def skip(v):
        return [c for c in (Comment, XComment, BComment)
                if isinstance(v, c)]

    def walk(ast):
        #print('walk:', ast)
        if [_ for _ in (Sharp, Quote, IQuote, UQuote, SUQuote)
            if isinstance(ast, _)]:
            revalue = walk_help(ast.value, walk)
            ast.value = revalue
        elif isinstance(ast, _LBase):
            # FIXME this kind of defeats the point of having
            # that nice pda in the first place when we recursively
            # descend here
            recollect = [walk_help(_, walk) for _ in ast.collect
                         if not skip(_)]
            ast.collect = recollect  # FIXME not quite right

        return walk_help(ast, None)
        # FIXME TODO quote and such
        #if ast.__class__ in mawp:
            #callable_thing = mawp[ast.__class__]
            #return callable_thing(ast)
        #else:
            #return ast

    def read(gen):
        exp_gen = parser(gen)
        for expression in exp_gen:
            if not skip(expression):
                transformed = walk_help(expression, walk)
                yield transformed

    return read


class _m:
    """ helper methods"""

    def eq_value(self, other):
        return type(self) == type(other) and self.value == other.value

    def eq__value(self, other):
        return type(self) == type(other) and self._value == other._value

    def eq_collect(self, other):
        # FIXME interned vs uninterned
        return type(self) == type(other) and self.collect == other.collect


# abstract syntax tree node types

class Ast:

    def __repr__(self, **kwargs):
        if hasattr(self, 'value') and type(self.value) != type(self.__repr__):
            if isinstance(self.value, String):
                return f'<{self.__class__.__name__[:2]} {self.value!r}>'
            else:
                return f'<{self.__class__.__name__[:2]} {self.value}>'
        if hasattr(self, '_value'):
            return f'<{self.__class__.__name__[:2]} {self._value}>'
        elif hasattr(self, 'collect'):
            return f'<{self.__class__.__name__[:3]} {self.collect!r}>'
        else:
            #self.__class__.__repr__ = lambda s: 'debug'
            #breakpoint()
            raise Exception('oops')

    def __hash__(self):
        return hash((self.__class__, self.value))

        # Atom

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

        # Keyword

        # FIXME need clarity on what identity model we follow here wrt
        # where Keyword is defined due to python issues with __main__
        # vs module imports keywords are self interning or somthing
        # like that so they are not as big an issue as symbols/atoms


class _LBase(Ast):

    __eq__ = _m.eq_collect

    @classmethod
    def from_elements(cls, *elements):
        return cls(list(elements))

    def __init__(self, collect):
        self.collect = collect

    def __repr__(self):
        return f'<{self._o} {repr(self.collect)[1:-1]} {self._c}>'


class ListP(_LBase):
    _o, _c = '()'


class ListS(_LBase):
    _o, _c = '[]'


class ListC(_LBase):
    _o, _c = '{}'


class Keyword(Ast):

    # swindle uses
    # #: -> real-keyword
    # : -> keyword
    # ' -> symbol

    # FIXME pretty sure we should get rid of these in the read pass
    # and let the individual implementations determine? but trickly
    # the tradeoff is that we have to make sure that we can
    # distinguish :|a b| and |:a b| internally if we read both
    # of them into Atom

    __eq__ = _m.eq_value

    @classmethod
    def from_collect(cls, collect):
        self = cls(''.join(collect))
        self.collect = collect
        return self

    def __init__(self, value):
        self.value = value


class EKeyword(Keyword):

    __eq__ = _m.eq_collect

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_ak_escape):
        if dialect_ak_escape is None:
            dialect_ak_escape = dialect_ak_escape_def

        return dialect_ak_escape(self.collect)


class Atom(Ast):
    """ Numbers, identifiers, booleans, and more.
        These are left uninterpreted. """

    __eq__ = _m.eq_value

    def __init__(self, collect):
        self._collect = collect
        self.value = ''.join(collect)
        # TODO convert the atoms to python types
        '#t' '#f' 't' 'nil' '1234'


def dialect_ak_escape_def(c):
    return ''.join([_.value
                    if isinstance(_, Escape)
                    else _ for _ in c])


class EAtom(Atom):
    # TODO FIXME figure out whether EThings are really subClassOf Things
    # or whether that will cause issues, in a sense, yes they are Chars
    # but they have different behavior and requirements

    __eq__ = _m.eq_collect

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_ak_escape=None):
        if dialect_ak_escape is None:
            # these nearly always go to identifiers I think?
            dialect_ak_escape = dialect_ak_escape_def

        return dialect_ak_escape(self.collect)


class _EscBase(Ast):

    __eq__ = _m.eq__value

    def __init__(self, value):
        self._value = value

    def __hash__(self):
        return hash((self.__class__, self._value))


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

    def value(self, dialect_escapes):
        return dialect_escapes(self._value)


class String(str, Ast):
    """ Ast for plain strings so that we can
        do things like track start/end """

    def __repr__(self):
        return dumps(self)


class EString(Ast):
    """ This is an unprocessed string that is still
    made out of its various parts. If you encounter
    an ast node of this type it should contain escaped
    charachters that need to be processed. """

    __eq__ = _m.eq_collect

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_escapes):
        ''.join([c.value(dialect_escapes)
                 if isinstance(c, SEscape)
                 else c for c in self.collect])


class Char(Ast):

    __eq__ = _m.eq__value

    def __init__(self, collect):
        self._value = ''.join(collect)

    def value(self, dialect_char):
        return dialect_char(self._value)


class EChar(Char):
    """ chars with escape sequences """

    __eq__ = _m.eq_collect

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_char, dialect_escapes):
        value = ''.join([c.value(dialect_escapes)
                         if isinstance(c, SEscape)
                         else c for c in self.collect])

        return dialect_char(value)


class Comment(Ast):
    """ Line comment """

    __eq__ = _m.eq_value

    def __init__(self, collect):
        self.collect = collect
        self.value = ''.join(collect)


class BComment(Ast):
    """ Block comment """

    __eq__ = _m.eq_value

    def __init__(self, collect):
        self.collect = collect
        self.value = ''.join([v._nest if isinstance(v, self.__class__) else v for v in collect])

    @property
    def _nest(self):
        return f'#|{self.value}|#'


class WrapsNext(Ast):

    __eq__ = _m.eq_value

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


class Quote(WrapsNext):
    """ HI there. """


class IQuote(WrapsNext):
    """ Quasiquote """


class UQuote(WrapsNext):
    """ Unquote """


class SUQuote(WrapsNext):
    """ Splicing Unquote """


class Sharp(WrapsNext):
    """ Sharp! Hash! Octothorp! Who knows. """


class XComment(WrapsNext):
    """ eXpression comments, aka datum comments """


class FeatureExpr(WrapsNext):
    """ Common Lisp feature expressions """


# second phase abstract types (incomplete)
# not clear we need this, I think it is probably
# one layer of indirection too many


class Identifier(Ast):

    __eq__ = _m.eq_value

    def __init__(self, value):
        self.value = value


class Syntax(Ast):

    __eq__ = _m.eq_value

    def __init__(self, value):
        self.value = value



class DataType(Ast):  # FIXME naming
    """ objects that have specific semantics in certain dialects
    this provides a layer of indirection so that the choice of
    the python structure used internally can be tailored to the
    exact use case without blindly following upstream """

    def caste(self, typef):
        return typef(self.collect)


class Cons(DataType):
    """ A single cell in a singly linked list, usually mutable. """


class LLike(DataType):

    o, c = None, None
    __eq__ = _m.eq_collect

    def __repr__(self, depth=0, **kwargs):
        # already have a printer for this in protcur
        r = '\n'.join(repr(_) for _ in self.collect)
        return self.o + r + self.c


class List(LLike):
    """ Usually a mutable singly linked list. """

    o, c = '()'

    def __init__(self, collect):
        self.collect = collect


class Array(LLike):
    """ An n-dimensional array. """

    o, c = '(array ', ')'

    def __init__(self, collect):
        self.collect = collect


class Vector(LLike):
    """ A one dimensional array of fixed length.
    Access is expected to be O(1). """

    o, c = '[]'

    def __init__(self, collect):
        self.collect = collect


class Set(LLike):
    """ A mathematical set. """

    o, c = '#{', '}'

    def __init__(self, collect):
        self.collect = collect


class Dict(LLike):
    """ Sometimes called a hash, sometimes called a map.
    Expectation is that access is O(1). """

    o, c = '{', '}'

    def __init__(self, collect):
        self.collect = collect


# class for pda states

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


# the unparsable char

unp = object()  # unparsable  # cannot use None

# helper for parsing elisp chars

escape_hat = SEscape('^')  # needed to parse elisp escape for control ?\^?

# tokens

toks_common = {
    't_newline':         '\n',
    't_tab':             '\t',
    't_space':           ' ',
    't_beg_list_p':      '(',
    't_end_list_p':      ')',
   #'t_beg_list_s':      '[',
   #'t_end_list_s':      ']',
   #'t_beg_list_c':      '{',
   #'t_end_list_c':      '}',
    't_beg_end_str':     '"',
   #'t_beg_end_aver':    '|',
    't_to_sharp':        '#',
   #'t_to_char_in_shrp': '\\',  # TODO
   #'t_to_cblk_in_shrp': '|',
   #'t_to_xcom_in_shrp': ';',
   #'t_to_keyw_in_shrp': ':',
    't_to_quote':        "'",
    't_to_quasi':        '`',
    't_to_unquote':      ',',
    't_to_splc_in_unq':  '@',
   #'t_to_keyw':         ':',  # FIXME racket and scheme keywords don't do this
   #'t_to_esc':          '\\',
    't_to_esc_in_str':   '\\',
   #'t_to_char':         '?',
    't_to_comment':      ';',
}

toks_b_list_s = {
    't_beg_list_s': '[',
    't_end_list_s': ']',
}

toks_b_list_c = {
    't_beg_list_c': '{',
    't_end_list_c': '}',
}

toks_b_list_sc = {
    **toks_b_list_s,
    **toks_b_list_c,
}

toks_scheme = {
    **toks_common,
    **toks_b_list_s,
    't_to_esc':          '\\',
    't_to_xcom_in_shrp': ';',
    't_beg_end_aver':    '|',
    't_to_cblk_in_shrp': '|',
    't_to_keyw_in_shrp': ':',
}

# dialect configuration

conf_sxpyr = {
    'additional_whitespace': ',',
    **toks_common,
    **toks_b_list_sc,
    't_to_keyw':         ':',
    't_to_xcom_in_shrp': ';',
    't_to_unquote':      '~',  # comma as whitespace is too useful
    # no block quotes no escapes and no verbatim atoms
}

conf_cl = {
    **toks_common,
    't_to_keyw':         ':',
    't_to_esc':          '\\',
    't_beg_end_aver':    '|',
    't_to_cblk_in_shrp': '|',
    # NOTE: do not use to_keyw_in_shrp for uninterned
    # it is simple to use (sharp (keyword ...)) than
    # to try to reconfigure this part of the reader
}

conf_el = {
    **toks_common,
    **toks_b_list_s,
    't_to_keyw': ':',
    't_to_char': '?',
    't_to_esc':  '\\',
}

conf_hy = {
    **toks_common,
    **toks_b_list_sc,
    't_to_keyw':         ':',
    't_to_xcom_in_shrp': '_',
    't_to_unquote':      '~',
}

conf_clj = {
    'quote_in_symbol': True,
    'additional_whitespace': ',',
    **toks_common,
    **toks_b_list_sc,
    't_to_keyw':         ':',
    't_to_char':         '\\',
    't_to_xcom_in_shrp': '_',
    't_to_unquote':      '~',
}

conf_rkt = {
    **toks_scheme,
}

conf_gui = {
    'quote_in_symbol': True,
    **toks_scheme,
}


# reader config

mawp_def = {
    ListP: lambda l: List(l.collect),
    ListS: lambda l: Vector(l.collect),
    ListC: lambda l: Dict(l.collect),
    EAtom: lambda a: Identifier(a.value()),  # FIXME not sure if correct
    EKeyword: lambda k: Keyword(k.value()) ,
    Atom: lambda a: Identifier(a.value),  # FIXME TODO numbers bools etc.
    #EString: Destring,
    #EChar: Dechar,
    #Char: lchar,
}


def walk_sxpyr(ast, walk):
    pass


def walk_rkt(ast, walk):
    # FIXME I really do not like this pattern
    # having to know about collect and value
    # is beyond annoying
    if isinstance(ast, Atom):
        v = ast.value
        if v[0].isdigit():
            try:
                # FIXME massive completely incorrect hack
                nv = literal_eval(v)  # lol so dumb
                return nv
            except Exception as e:
                pass

        return Identifier(v)
    elif isinstance(ast, Sharp):
        v = ast.value  # sigh case statements would be lovely
        if isinstance(v, Atom):
            if v.value == 't':
                return True
            elif v.value == 'f':
                return False
        if isinstance(v, Keyword):
            # FIXME and here the inheritance hierarchy returns
            # to ruin your day
            return Keyword(v.value)
        #elif isinstance(v, Identifier):  # Atom converted before we get here
        elif isinstance(v, Quote):
            return Syntax(v)
    elif isinstance(ast, EChar):
        pass
    elif isinstance(ast, Char):
        pass
    elif walk is None:  # post process after doing members
        if isinstance(ast, _LBase):
            return List(ast.collect)
    else:
        return walk(ast)

    return ast


mawp_rkt = {
    **mawp_def,
    ListP: lambda l: List(l.collect),
    ListS: lambda l: List(l.collect),
    ListC: lambda l: List(l.collect),
    #Sharp: v_rkt,  # FIXME indication that we don't need mawp, we just need walk_help?
    #Atom: v_rkt,
}

def configure(quote_in_symbol=False,  # True, False, SyntaxError
              additional_whitespace='',
              #escape_always=True,
              #curlies_map=False,
              #char_esc_expand=tuple(),  # expand rules must be provided at runtime FIXME dict or function?
              char_auto_escape=True,  # FIXME the variety of behaviors has 2 axes I think, not clear what the default should be yet
              char_auto_end=True,  # FIXME same issue  XXX not sure we need this, somehow I think racket special cased these?
              #char_name_symbol=False,  # this is what cl does
              #immutable_cons=False,  # FIXME this should be deferred
              sharp_atom_end_only_some=False,  # FIXME I think this is how to deal w/ racket?

              ## tokens (common are set)

              # the naming conventions for tokens
              # are to use beg end when a form must end with a specific token
              # and to use to for forms that have multiple possible ends

              t_newline='\n',
              t_tab='\t',
              t_space=' ',
              t_beg_list_p='(',
              t_end_list_p=')',
              t_beg_list_s=unp,
              t_end_list_s=unp,
              t_beg_list_c=unp,
              t_end_list_c=unp,
              t_beg_end_str='"',
              t_beg_end_aver=unp,
              t_to_sharp='#',
              t_to_cblk_in_shrp=unp,
              t_to_xcom_in_shrp=unp,
              t_to_keyw_in_shrp=unp,
              t_to_quote="'",
              t_to_quasi='`',
              t_to_unquote=',',
              t_to_splc_in_unq='@',
              t_to_keyw=unp,
              t_to_esc=unp,
              t_to_esc_in_str='\\',
              t_to_char=unp,
              t_to_comment=';',

              **kwargs):

    eof = object()
    toks = [t_newline, t_tab, t_space,
            t_beg_list_p, t_end_list_p,
            t_beg_list_s, t_end_list_s,
            t_beg_list_c, t_end_list_c,
            t_beg_end_str, t_beg_end_aver,
            t_to_sharp, t_to_quote, t_to_quasi, t_to_unquote, 
            t_to_keyw, t_to_esc, t_to_char, t_to_comment,]

    # even though additional whitespace doesn't get an explicit name
    # it still needs to be in syms to prevent it from being included
    # in identifier symbols
    toks.extend(additional_whitespace)

    # other symbols that we may want to support directly in the reader
    #am = '&'  # maybe want for lambda list control words?
    #qm = '?'
    #at = '@'  # TODO splicing unquote

    whitespace = (t_newline, t_tab, t_space) + tuple(_ for _ in additional_whitespace)
    scoe_complex = (t_newline, t_beg_end_str, t_to_esc, t_to_sharp, t_to_cblk_in_shrp)
    # note that pipe does not end atoms and keywords but the state still
    # transitions to v similarly exiting v does not exit atom or keyword
    ak_ends = (t_beg_list_p, t_end_list_p, t_beg_list_s, t_end_list_s, t_beg_list_c, t_end_list_c, t_to_comment, t_beg_end_str, t_to_quote, t_to_quasi, t_to_unquote, *whitespace)
    '()[]{};"\'`,\n\t '
    if quote_in_symbol:
        ak_ends = tuple(s for s in ak_ends if s != t_to_quote)

    m_ends = (*ak_ends, t_to_char, t_beg_end_aver, t_to_sharp)  # FIXME tricky because we need at least one char for m

    # XXX NOTE state could be defined globally but I think
    # keeping it in here will have better performance
    states = [bos, s_atom, s_keyw, s_quote, s_unquote, s_unq_first,
              s_unq_splice, s_quasiq, s_string, s_list_p, s_list_s,
              s_list_c, s_sharp, s_comm, s_esc, s_esc_str,
              s_comment_x, s_feat_x, s_cblk, s_pi_in_cblk,
              s_atom_verbatim, s_comment_b_nest, s_char, s_char_first] = [
        State(i) for i in range(24)]
    esclikes = s_esc, s_char
    quotelikes = s_quote, s_unquote, s_unq_first, s_unq_splice, s_quasiq, s_comment_x, s_feat_x, s_sharp
    listlikes = s_list_p, s_list_s, s_list_c, s_cblk

    state_names = {
        bos: 'bos',
        s_atom: 'atom',
        s_keyw: 'keyw',
        s_quote: 'quot',
        s_unquote: 'unqu',
        s_unq_first: 'unqf',
        s_unq_splice: 'unqs',
        s_quasiq: 'qasi',
        s_string: 'strn',
        s_list_p: 'list',
        s_list_s: 'vect',
        s_list_c: 'dict',
        s_sharp: 'shrp',
        s_comm: 'comm',
        s_esc: 'escp',
        s_esc_str: 'sesc',
        s_comment_x: 'xcom',
        s_feat_x: 'feat',
        s_cblk: 'cblk',
        s_pi_in_cblk: 'mebl',  # maybe leave block comment
        s_atom_verbatim: 'aver',
        s_comment_b_nest: 'nblk',
        s_char: 'char',
        s_char_first: 'chrf',
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

    def caste_string(collect):
        try:
            return String(''.join(collect))  # FIXME syntax loc
        except TypeError:
            return EString(collect)

    def caste_atom(collect):
        return (EAtom(collect) if
                [_ for _ in collect if isinstance(_, Escape)] else
                Atom(collect))

    def caste_keyword(collect):
        return (EKeyword(collect) if
                [_ for _ in collect if isinstance(_, Escape)] else
                Keyword.from_collect(collect))

    def caste_sharp(collect):
        if collect:
            return Sharp.from_collect(collect)  # TODO override
        else:
            # defer error until later and
            # note that this is not an EAtom
            return Atom('#')

    def caste(state, collect):  # TODO start and end points
        # FIXME this second set of branches shouldn't be necessary?
        if   state == s_string: val = caste_string(collect)
        elif state == s_atom: val = caste_atom(collect)
        elif state == s_comm: val = Comment(collect)
        elif state == s_cblk: val = BComment(collect)
        elif state == s_keyw: val = caste_keyword(collect)
        elif state == s_list_p: val = ListP(collect)
        elif state == s_list_s: val = ListS(collect)
        elif state == s_list_c: val = ListC(collect)
        elif state == s_quote: val = Quote.from_collect(collect)
        elif state == s_quasiq: val = IQuote.from_collect(collect)
        elif state == s_unquote: val = UQuote.from_collect(collect)
        elif state == s_unq_splice: val = SUQuote.from_collect(collect)
        elif state == s_sharp: val = caste_sharp(collect)
        elif state == s_comment_x: val = XComment.from_collect(collect)
        elif state == s_feat_x: val = FeatureExpr.from_collect(collect)  # TODO override
        elif state == s_atom_verbatim: val = caste_atom(collect)  # cl vs rr different behavior '|a \ b| '|a  b|
        elif state in (s_char_first, s_char): val = (EChar(collect)
                                      if [_ for _ in collect if isinstance(_, SEscape)] else
                                      Char(collect))
        # XXX if you hit this error with bos at eof DO NOT ADD bos here!
        else: raise NotImplementedError(f'Unknown state: {state} {collect}')
        # bind start and end points here for simplicity
        # val.start, val.end = start, end
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
        # worth the function call overhead for readability
        def to_state(next_state):
            stack.append(next_state)
            collect_stack.append([])
            return None

        def to_state_collect(next_state):
            stack.append(next_state)
            _collect = []
            collect_stack.append(_collect)
            return _collect

        if   char == t_beg_end_str: collect = to_state_collect(s_string)
        elif char == t_to_comment: collect = to_state_collect(s_comm)
        elif char == t_beg_list_p: collect = to_state(s_list_p)
        elif char == t_beg_list_s: collect = to_state(s_list_s)
        elif char == t_beg_list_c: collect = to_state(s_list_c)
        elif char == t_to_quote: collect = to_state(s_quote)
        elif char == t_to_quasi: collect = to_state(s_quasiq)
        elif char == t_to_unquote: collect = to_state(s_unq_first)
        elif char == t_beg_end_aver: collect = to_state_collect(s_atom_verbatim)
        # XXX warning: this branch for pipe only works correctly if
        # collect is None
        elif char == t_to_sharp:
            state = stack[-1]
            if state == s_cblk:  # FIXME do we really want to do this here
                # it means that we didn't handle a case correctly in parse
                stack.append(s_comment_b_nest)  # FIXME maybe nest s_cblk
            else:
                collect = to_state(s_sharp)
        elif char == t_to_char:
            collect = to_state_collect(s_char_first)
        elif char == t_to_esc_in_str:
            collect = to_state_collect(s_esc_str)
        else:
            raise Exception('This should never happen.')

        return collect


    def resolve_pops(collect_stack, collect, stack, char):
        state = stack[-1]
        *cut, = i_resolve_pops(collect_stack, stack, char)

        if state == s_char_first:  # FIXME using this to prevent issues with strings sigh
            collect = None
        elif (char in (t_beg_list_p, t_beg_list_s, t_beg_list_c, t_to_quote, t_to_quasi, t_to_unquote) or
              state == s_char and char in (t_to_char, t_to_sharp) or  # and char_auto_end ??? chars may end chars ie not cl
            char == t_beg_end_str and state != s_string):
            collect = push_state_from_char(collect_stack, collect, stack, char)
        elif stack[-1] == s_cblk:
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
        if (state_prev in (s_pi_in_cblk,)):  # transition only no collect
            return i_resolve_pops(collect_stack, stack, char)

        # FIXME #\;a causes isses for a bunch of readers which
        # this behavior avoids, but requires a special case
        #if state_prev == m and char == sc:

        cut = process_collect(collect_stack, state_prev)  # cut /
        state = stack[-1]
        ##print(f'resp: {state_prev} {state} {cut!r}')

        if state_prev in listlikes and char in (t_end_list_p, t_end_list_s, t_end_list_c):
            char = None

        if char == eof:  # sigh using a branch for something that provably only happens once
            if state_prev in (s_string, s_atom_verbatim, *listlikes):
                # TODO unterminated x starting at position y

                raise SyntaxError(f'Unterminated thing {state_prev}.')

            if (state != bos or #and state_prev in (c, m, a) or
                state_prev == s_char_first):
                collect_stack[-1].append(cut)
                return i_resolve_pops(collect_stack, stack, char)

            return cut,

        elif not collect_stack:
            return cut,
        elif state_prev == s_comm:
            if state in (*quotelikes, *listlikes):
                collect_stack[-1].append(cut)
                return tuple()
            else:
                return cut,
        elif state in quotelikes:
            # FIXME make sure features eat twice?
            collect_stack[-1].append(cut)
            if state_prev is s_comment_x and state is s_comment_x:
                # don't comment things that are already commented
                # comment the thing that comes after them
                # FIXME this gets very tricky with feature expressions
                return tuple()

            return i_resolve_pops(collect_stack, stack, char)
        elif (char in (t_end_list_p, t_end_list_s, t_end_list_c) and
              state_prev in (*quotelikes, s_atom, s_keyw, s_char, s_char_first)):
            # multiple things end at the same time
            collect_stack[-1].append(cut)
            if state_prev != s_char_first:
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
        if char in (t_end_list_p, t_end_list_s, t_end_list_c): raise SyntaxError(f'# {char} not allowed.')
        elif char == t_to_cblk_in_shrp:  # FIXME essentially things that we want to ignore right now
            stack.pop()
            stack.append(s_cblk)
            # FIXME need to update collect
            #collect_stack.pop()
            return True
        elif char == t_to_esc:
            stack.pop()
            stack.append(s_char_first)
            return True
        elif char == t_to_xcom_in_shrp:
            stack.pop()
            # do not need collect_stack
            # reuse the collect from h for x
            stack.append(s_comment_x)
            return True
        elif char in '+-':
            stack.pop()
            # do not need collect_stack
            # reuse the collect from h for f
            stack.append(s_feat_x)
            return True


    def parse(iter_over_len_1_strings):
        """ First phase parse into a common representation
        of a generic lisp AST. A second, dialect specific
        phase is needed to complete the functionality needed
        for read. """
        bp = False
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

        def mgen():
            nonlocal line
            # We use a generator function here so that if the
            # generator is passed to a dispatch macro form all the
            # book keeping machinery stays in sync.
            for point, char in enumerate(iter_over_len_1_strings):
                yield point, char
                if char == t_newline:
                    line += 1

        gen = mgen()
        try:
            for point, char in gen:
                state = stack[-1]

                if debug:
                    print('cs:', char, stack)
                    pass

                if state == s_unq_first:
                    # FIXME can we integrate this into the main switch?
                    if char == t_to_splc_in_unq:
                        stack.pop()
                        stack.append(s_unq_splice)
                        continue
                    else:
                        stack.pop()
                        stack.append(s_unquote)
                        state = s_unquote

                if state == s_sharp:
                    # make any transitions that are required,
                    # fail on failure cases, or proceed
                    # needs to come before escape due to #\
                    if process_h(collect_stack, stack, char):
                        if stack[-1] in (s_cblk, s_char_first):
                            collect = collect_stack[-1]  # FIXME h should be via push_state_from_char no?
                        continue

                # handle escape
                if state in (s_esc, s_esc_str):
                    # escape is only needed for a single char
                    stack.pop()
                    # FIXME TODO add explicit Ast node for this
                    # so we can differentiate between the raw forms?
                    # yes, this is better than the alternative which
                    # is to hardcode escape rules into the reader
                    if state == s_esc_str:
                        #char = caste(state, char)  # TODO make homogenous
                        char = SEscape(char)
                        #breakpoint()
                    else:
                        char = Escape(char)
                elif t_to_char == '?' and state in (s_char_first, s_char) and char == t_to_esc:  # FIXME HACK for elisp
                    stack.append(s_esc_str)  # NOTE elisp uses the same escape sequences for chars and strings
                    continue
                elif state not in (s_comm, s_cblk, s_comment_b_nest, s_char_first) and char == t_to_esc:  # implicit not in (c, o, m1)
                    # FIXME TODO need to split \ in string from the rest
                    if state in (s_string, s_char):
                        stack.append(s_esc_str)
                    else:
                        stack.append(s_esc)
                    #breakpoint()  # XXX m debug start point
                    # FIXME interaction between m and e
                    continue
                elif state == s_string and char == t_to_esc_in_str:
                    # FIXME we should be able to handle this homogenously
                    stack.append(s_esc_str)
                    continue

                if state == s_char_first:
                    if char in toks and char in m_ends:
                        collect.append(char)
                    else:
                        # FIXME action at a distance
                        # this pops here to keep the scoe
                        # block a bit simpler
                        stack.pop()

                if state in (s_esc, s_esc_str) and stack[-1] == s_char_first: # state == m or
                    # FIXME m and e have sligh issues here
                    # needs to be handled after e
                    # ideally we could fall through
                    # since it pushes and pops on the same char
                    # but that means we would have to do something
                    # to reorder everything
                    # FIXME sigh, so now we copy and paste all the stuff from
                    # our usual resolve pops section here

                    collect = collect_stack[-1]  # inherit from the sharp or question mark?
                    collect.append(char)

                    if char not in toks or char not in m_ends:
                        stack.pop()
                        stack.append(s_char)

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

                if state == s_comment_b_nest:
                    stack.pop()
                    if char == t_to_cblk_in_shrp:
                        # FIXME sigh
                        stack.append(s_cblk)
                        collect = []
                        collect_stack.append(collect)
                    else:
                        collect.append(t_to_cblk_in_shrp)

                elif state == s_cblk and char == t_to_cblk_in_shrp:
                    stack.append(s_pi_in_cblk)

                # handle all the + cases except
                elif (char not in toks or
                      state in (s_atom, s_keyw) and char not in ak_ends and char != t_beg_end_aver or
                      state in (s_char_first, s_char) and char not in m_ends or
                      char == t_to_keyw or
                      state != s_unq_first and char == t_to_splc_in_unq or  # FIXME do we really need u1?
                      state == s_sharp and char in (t_to_sharp, t_to_keyw_in_shrp) or  # FIXME should probably work like an FeaureExpr?
                      state == s_atom_verbatim and char != t_beg_end_aver or
                      state == s_pi_in_cblk and char != t_to_sharp or  # fixme confusing
                      state in (s_string, s_comm, s_cblk, s_esc, s_esc_str) and char not in scoe_complex or
                      state in (s_string, s_comm,         s_esc, s_esc_str) and char in (t_to_sharp, t_to_cblk_in_shrp) or
                      state in (s_string,         s_cblk, s_esc, s_esc_str) and char == t_newline or
                      state in (          s_comm, s_cblk, s_esc, s_esc_str) and char in (t_beg_end_str, t_to_esc) or
                      (t_to_char == '?' and
                       # FIXME EEEEMMMMMAAAAAAAAAAAAACCCCCCSSSSSS
                     state == s_char and
                       char in m_ends and
                       (collect[-1] == escape_hat or
                        collect[-1] == '-' and
                        len(collect) > 1 and  # FIXME HACK to avoid index error should be a state
                        # implicitly checking if there is a KeyChar in collect
                        # TODO see if we accidentally induce bugs by reading
                        # things that the elisp reader would not
                     isinstance(collect [-2], SEscape)))):
                    if collect is None:  # if collect is a list it is
                        # for another state so safe to assign state
                        # here, also required to handle the case where
                        # the loop immediately terminates
                        state = s_keyw if char in (t_to_keyw, t_to_keyw_in_shrp) else s_atom
                        stack.append(state)
                        collect = []
                        collect_stack.append(collect)

                        if point_start is None:
                            point_start = point

                        if state == s_keyw:  # don't collect the colon that starts the keyword
                            continue

                    if state == s_pi_in_cblk and char != t_to_cblk_in_shrp:
                        stack.pop()
                        collect.append(t_to_cblk_in_shrp)
                    elif state == s_char_first:
                        stack.append(s_char)  # FIXME lurking transition

                    collect.append(char)
                    # FIXME if we are in state m and get here everything breaks
                    # this is where a goto would be really easy

                elif ((state in quotelikes or state == bos)
                                        and char in (t_end_list_p, t_end_list_s, t_end_list_c) or
                      state == s_list_p and char in (              t_end_list_s, t_end_list_c) or
                      state == s_list_s and char in (t_end_list_p,               t_end_list_c) or
                      state == s_list_c and char in (t_end_list_p, t_end_list_s)):
                    raise SyntaxError('No matching paren!')

                # majority of pops happen here
                elif (state in (s_atom, s_keyw) and char in ak_ends or
                      state in (s_char_first, s_char) and char in m_ends or
                      state == s_list_p     and char == t_end_list_p or
                      state == s_list_s     and char == t_end_list_s or
                      state == s_list_c     and char == t_end_list_c or
                      state == s_string     and char == t_beg_end_str or
                      state == s_comm       and char == t_newline or
                      state == s_pi_in_cblk and char == t_to_sharp):
                    collect, *thing = resolve_pops(collect_stack, collect, stack, char)
                    yield from thing
                    # FIXME not sure if correct
                    if thing:
                        old_ps = point_start  # for debug
                        point_start = None
                    if collect is not None:
                        point_start = point

                elif char == t_beg_end_aver:  # FIXME bad implementation should not need this special case
                    # pi and ha are cases where we might or might not want/need to updated collect
                    # which is why they are sort of separate because we stopped passing collect
                    # through push_state_from_char
                    if state == s_atom_verbatim:
                        stack.pop()
                        state = stack[-1]
                    else:
                        # yep, just as complex as expected :/
                        if state in listlikes or state in quotelikes or state in (s_atom, s_keyw, bos):
                            if collect is None:
                                stack.append(s_atom)  # : must come outside if you want a keyword
                                collect = push_state_from_char(collect_stack, collect, stack, char)
                            else:
                                stack.append(s_atom_verbatim)
                        if point_start is None:
                            point_start = point

                elif state == s_cblk and char == t_to_sharp:
                    push_state_from_char(collect_stack, collect, stack, char)

                elif (state in listlikes or
                      state in quotelikes or
                      state == bos):
                    if char in whitespace:
                        continue

                    collect = push_state_from_char(collect_stack, collect, stack, char)
                    if point_start is None:
                        point_start = point
                else:
                    raise Exception("I can't believe you've done this")

            if collect_stack:  # EOF case
                collect, *thing = resolve_pops(collect_stack, collect, stack, eof)
                yield from thing
                if stack and stack != [bos]:
                    # handle this special case out here to avoid
                    # repeatedly checking if we are at eof internally
                    raise Exception('NOT DONE YET')

        except Exception as err:
            if point_start is None:
                point_start = old_ps
            ps = point_start + 1 if point_start is not None else 0
            pe = point + 1 if point is not None else 0
            msg = f'error start {ps} end {pe} line {line}'
            raise err.__class__(msg) from err


    return parse
