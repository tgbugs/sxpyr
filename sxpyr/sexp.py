__version__ = '0.0.1'

from types import GeneratorType


debug = False


class _m:
    """ helper methods"""

    def eq_value(self, other):
        return type(self) == type(other) and self.value == other.value

    def eq__value(self, other):
        return type(self) == type(other) and self._value == other._value

    def eq_collect(self, other):
        # FIXME interned vs uninterned
        return type(self) == type(other) and self.collect == other.collect


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
    # FIXME pretty sure we should get rid of these in the read pass
    # and let the individual implementations determine? but trickly
    # the tradeoff is that we have to make sure that we can
    # distinguish :|a b| and |:a b| internally if we read both
    # of them into Atom

    __eq__ = _m.eq_value

    def __init__(self, collect):
        self.collect = collect
        self.value = ''.join(collect)


class EKeyword(Keyword):

    __eq__ = _m.eq_collect

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_ak_escape):
        return dialect_ak_escape(collect)


class Atom(Ast):
    """ Numbers, identifiers, booleans, and more.
        These are left uninterpreted. """

    __eq__ = _m.eq_value

    def __init__(self, collect):
        self._collect = collect
        self.value = ''.join(collect)
        # TODO convert the atoms to python types
        '#t' '#f' 't' 'nil' '1234'


class EAtom(Atom):
    # TODO FIXME figure out whether EThings are really subClassOf Things
    # or whether that will cause issues, in a sense, yes they are Chars
    # but they have different behavior and requirements

    __eq__ = _m.eq_collect

    def __init__(self, collect):
        self.collect = collect

    def value(self, dialect_ak_escape):
        return dialect_ak_escape(collect)


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


class String(Ast, str):
    """ Ast for plain strings so that we can
        do things like track start/end """


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
    """ TODO chars with escape sequences in them """

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
                Keyword(collect))

    def caste_sharp(collect):
        if collect:
            return Sharp.from_collect(collect)  # TODO override
        else:
            # defer error until later and
            # note that this is not an EAtom
            return Atom('#')

    def caste(state, collect):  # TODO start and end points
        # FIXME this second set of branches shouldn't be necessary?
        if   state == s: val = caste_string(collect)
        elif state == a: val = caste_atom(collect)
        elif state == c: val = Comment(collect)
        elif state == blk: val = BComment(collect)
        elif state == k: val = caste_keyword(collect)
        elif state == t: val = ListP(collect)
        elif state == l: val = ListS(collect)
        elif state == d: val = ListC(collect)
        elif state == q: val = Quote.from_collect(collect)
        elif state == i: val = IQuote.from_collect(collect)
        elif state == u: val = UQuote.from_collect(collect)
        elif state == us: val = SUQuote.from_collect(collect)
        elif state == h: val = caste_sharp(collect)
        elif state == x: val = XComment.from_collect(collect)
        elif state == f: val = FeatureExpr.from_collect(collect)  # TODO override
        elif state == v: val = caste_atom(collect)  # cl vs rr different behavior '|a \ b| '|a  b|
        elif state in (m1, m): val = (EChar(collect)
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
        if char in (cp, cs, cc): raise SyntaxError(f'# {char} not allowed.')
        elif char == pih:  # FIXME essentially things that we want to ignore right now
            stack.pop()
            stack.append(blk)
            # FIXME need to update collect
            #collect_stack.pop()
            return True
        elif char == bs:
            stack.pop()
            stack.append(m1)
            return True
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

        def mgen():
            nonlocal line
            # We use a generator function here so that if the
            # generator is passed to a dispatch macro form all the
            # book keeping machinery stays in sync.
            for point, char in enumerate(iter_over_len_1_strings):
                yield point, char
                if char == nl:
                    line += 1

        gen = mgen()
        try:
            for point, char in gen:
                state = stack[-1]

                if debug:
                    print('cs:', char, stack)
                    pass

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

                if state == m1:
                    if char in syms and char in m_ends:
                        collect.append(char)
                    else:
                        # FIXME action at a distance
                        # this pops here to keep the scoe
                        # block a bit simpler
                        stack.pop()

                if state in (e, es) and stack[-1] == m1: # state == m or
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

                    if char not in syms or char not in m_ends:
                        stack.pop()
                        stack.append(m)

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
                        state = k if char == co else a
                        stack.append(state)
                        collect = []
                        collect_stack.append(collect)
                        if point_start is None:
                            point_start = point

                    if state == r and char != pih:
                        stack.pop()
                        collect.append(pih)
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
