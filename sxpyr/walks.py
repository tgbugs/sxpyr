from sxpyr._exports import *
from sxpyr.sxpyr import (
    Ast, Walk, _m,
    DispatchNotImplementedError)
from sxpyr import char
from unicodedata import name as uniname, lookup as unilookup


class WalkLazy(Walk):
    """ Bad implementations to get things working. """

    def atom(self, ast):
        v = ast.value
        if v[0].isdigit():
            try:
                # FIXME massive completely incorrect hack
                nv = literal_eval(v)  # lol so dumb
                return nv
            except Exception as e:
                pass

        return Identifier(v)


class WalkRkt(Walk):

    _recurse_funs = (
        (Syntax, '_quote'),
        (ISyntax, '_isyntax'),
    ) + Walk._recurse_funs

    _unquote_family = ('usyntax', 'susyntax') + Walk._unquote_family
    _uqf_funs = (
        (USyntax, '_uq_any'),
        (SUSyntax, '_uq_any'),
    ) + Walk._uqf_funs

    _type_funs = (
        (Syntax, 'syntax'),
        (ISyntax, 'isyntax'),
        (USyntax, 'usyntax'),
        (SUSyntax, 'susyntax'),
    ) + Walk._type_funs

    _sh_funs = (
        
    ) + Walk._sh_funs

    lists = Walk.listp
    listc = Walk.listp
    atom = WalkLazy.atom

    def __init__(self):
        super().__init__()

    def _iq_overwrite(self):
        super()._iq_overwrite()
        self.usyntax, self.susyntax = self.iq_ustx, self.iq_suns

    def _is_overwrite(self):
        self.uquote, self.suquote = self.is_uqot, self.is_sunq
        self.usyntax, self.susyntax = self.is_ustx, self.is_suns

    def _isyntax(self, ast):
        return self._quote_family_wrapper(ast, self._is_overwrite)

    def atom(self, ast):
        # NOTE since we don't have a full implementation we can't
        # define these dynamically so we hardcode them as errors here
        # this matches the behavior of calling read in racket
        if ast.value in ('#',):
            raise SyntaxError(f'Illegal atom {ast}')
        else:
            return super().atom(ast)

    def syntax   (self, ast): return ast
    def isyntax  (self, ast): return ast

    def is_ustx  (self, ast): return ast
    def is_suns  (self, ast): return ast
    def is_uqot  (self, ast): return ast
    def is_sunq  (self, ast): return ast

    def iq_ustx  (self, ast): return ast
    def iq_suns  (self, ast): return ast
    # NOTE unlike common lisp, racket only raises an error if the un
    # forms are used at the top level, if they are used inside of a
    # quoted form it will not complain not sure whether that is
    # intentional or not
    def usyntax  (self, ast):
        raise SyntaxError(f'unsyntax not in quasisyntax {ast}')
    def susyntax (self, ast):
        #breakpoint()
        raise SyntaxError(f'unsyntax splicing not in quasisyntax {ast}')

    #def sharp(self, ast):
    #    """ unspecified dispatch macros """
    #    value = ast.value
        
    def sh_atom(self, ast):
        value = ast.value
        if value == 't':
            return Boolean(True)
        elif value == 'f':
            return Boolean(False)
        elif value == 'lang':
            # FIXME ambiguity in the parser #lang at-exp racket/base
            # and other things means that the lang line has to be
            # handled directly in the reader which is extremely
            # annoying it should just slurp the next expression which
            # could be a listy thing #lang(at-exp racket/base) and
            # then it could be anywhere it would need to be clear that
            # it wasnot a function but it would definitely be more
            # consistent and easier to parse, not as aesthetic sigh
            # the balance of implementation difficulty vs additional
            # work for users vs consistency and predictability
            return LangLine('sigh-reader-is-broken-for-this')
        elif value in ('rx'):
            return Sharp(ast)  # FIXME TODO broken at the parse level
        elif value == 'rx#':  # FIXME bytestring
            # TODO in theory we could have these parse the way feature
            # expressions do so that when they terminate they don't
            # transition back to the top but instead to the OM NOM NOM
            # for the next single expression, of course the requirement
            # that there be no whitespace is artifical as far as I can
            # tell because racket always generats an error if these are
            # present alone this is true for #hash as well
            return Sharp(ast)  # FIXME TODO broken at the parse level
        elif value in ('px'):  # has different syntax perl maybe?
            return Sharp(ast)  # FIXME TODO broken at the parse level
        elif value == 'px#':  # FIXME bytestring
            return Sharp(ast)  # FIXME TODO broken at the parse level
        elif value.startswith('%'):
            # e.g. #%kernel
            return Identifier('#' + value)
        elif value.startswith('&'):
            return Box(value)
        elif value[0] in 'xdb':  # XXX warning risk of the empty atom
            return Number(ast)
        elif value == 's':
            return Struct(value)
        elif value.startswith('hash'): # FIXME FIXME
            return Dict(value)
        elif value.startswith('<<'):  # FIXME very broken
            return Sharp(ast)  # FIXME TODO broken at the parse level

        #elif value in ('',):

            # XXX incidentally the writing below is wrong, and the
            # way I wrote the reader you don't deal with a bare sharp
            # as a sharp itself, it is just an atom, we COULD implement
            # this as a Sharp ast node in caste_sharp, but it simplifies
            # things for the Sharp ast node to never be empty

            # a bare sharp is not an unbound identifier in racket it
            # is its own syntax error, that may be implemented as a
            # macro in the racket reader implementation but since we
            # don't have a complete runtime backing this we can't
            # quite pull that off yet, so we create explicit syntax
            # errors here instead, NOTE double sharp ## is not here
            # for now because the racket reader supports dispatch
            # macro chars

            #raise SyntaxError(f'Bad syntax {ast}')
        else:
            # TODO #-1 #1 #2
            # TODO arbitrary multidispatch key-5
            raise DispatchNotImplementedError(f'No dispatch transformer for {ast}')

    def sh_keyw (self, ast): return ast
    def sh_estr (self, ast):
        # TODO bytestring
        return ast
    def _sh_lst (self, ast): return Vector.from_ast(ast)
    def sh_lstp (self, ast): return self._sh_lst(ast)
    def sh_lsts (self, ast): return self._sh_lst(ast)
    def sh_lstc (self, ast): return self._sh_lst(ast)

    # FIXME need _sh_iquote in preprocessing
    def sh_quot (self, ast): return Syntax.from_ast(ast)
    def sh_iqot (self, ast): return ISyntax.from_ast(ast)
    def sh_uqot (self, ast): return USyntax.from_ast(ast)
    def sh_sunq (self, ast): return SUSyntax.from_ast(ast)

    def charspec(self, ast):
        # https://docs.racket-lang.org/reference/reader.html#%28part._parse-character%29
        value = ast.value
        if len(value) == 1:  # racket is utf8 aware by default
            # NOTE raw whitespace will not be printed the same
            # way it is in racket, but will match python repr
            nv = value
        elif value.lower() in char.known_multi:
            # racket does downcase these #\SpAcE
            nv = char.known_multi[value.lower()]
        elif value[0] == 'u' and len(value) <= 5:
            nv = chr(int(value[1:], base=16))
        elif value[0] == 'U' and len(value) <= 7:
            nv = chr(int(value[1:], base=16))
        elif value[0] in '0123' and len(value) == 3:
            # octal notation must have leading zeros in racket if it
            # is for chars, in strings it matches python
            i = int(value, base=8)
            if i > 255:
                # this matches python's behavior exactly but we have
                # to catch it here because python's chr function
                # supports arbitrary ints
                msg = ('Racket does not support octal notation '
                       'for chars longer than a single byte. '
                       'Aka 255, 0o377 or #\\377.')
                raise SyntaxError(msg)

            nv = chr(i)
        else:
            msg = f'unknown char spec #\\{value}'
            raise SyntaxError(msg)

        return Char(nv)


class WalkCl(Walk):
    def sh_lsts(self, ast):
        return Vector.from_ast(ast)


class CharEl(Ast):
    """ Emacs has some ... non-standard char representations. """

    def __init__(self, collect, elc_int=None):
        if collect and elc_int:
            raise TypeError("can't have both collect and elc_int")
        self.collect = collect
        self._elc_int = elc_int

    def __eq__(self, other):
        return (type(self) == type(other) and
                self.elc_int == other.elc_int)

    @property
    def elc_int(self):
        """ emacs char int """
        if self._elc_int is not None:
            return self._elc_int

        ints = [ord(c) if isinstance(c, str)
                else c.bit for c in self.collect]
        return sum(ints)


class WalkEl(Walk):

    @staticmethod
    def _compute_escape_value(value):
        if value in char.cee_el:
            return char.cee_el[value]

        return value

    def lists(self, ast):
        return Vector.from_ast(ast)

    def sh_lsts(self, ast):
        return ByteCode.from_ast(ast)

    def sh_atom(self, ast):
        # racket
        # https://docs.racket-lang.org/reference/reader.html#%28part._parse-number%29

        # cl
        # https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node17.html
        # see the compatibility note for further crazytown

        # el
        # https://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html

        # clj (doesn't go here, goes in regular atom)
        # https://clojure.org/guides/learn/syntax#_numeric_types

        # cl el racket scheme all seem to support this
        # hy supports python's number syntax 0b 0o 0x
        # clojure has some insane mishmash of java and sharpless cl style
        # #b -> bin literal
        # #o -> oct literal
        # #x -> hex literal

        # cl and el support #NrXXXX syntax
        return ast

    def charspec(self, ast):
        # the elisp unescaped char spec is actually pretty easy
        # everything else is hard
        value = ast.value
        if len(value) == 1:
            return Char(value)
        else:
            raise SyntaxError(f'Unknown char spec ?{value}')

    def echarspec(self, ast):
        collect = ast.collect
        c0 = collect[0]
        rest = collect[1:]
        if isinstance(c0, SEscape):
            v0 = c0.value(self._compute_escape_value)
            if isinstance(v0, type) and issubclass(v0, char.KeyChar):
                if v0 == char.SpaceOrSuperNeedDash and not rest:
                    nv = ' '  # spacceeee
                elif v0 == char.UnicodeName and rest[0] == '{' and rest[-1] == '}':
                    # this is where emacs allows variable length unicode
                    uniname = ''.join(rest[1:-1])
                    if uniname.startswith('U+'):
                        nv = chr(int(uniname[2:], base=16))
                    else:
                        try:
                            nv = unilookup(uniname)
                        except KeyError as e:
                            raise SyntaxError(f'unknown unicode name {uniname}')
                else:
                    # expect multiple escapes
                    # control-and-friends-char syntax
                    # emacs uses the 2**22 through 2**27 bits to
                    # mark that certain keys have been pressed
                    # NOTE this section is recursive, the final
                    # key of the combination can be represented
                    # however you like
                    dialect_escapes = self._compute_escape_value
                    hrm = [c.value(dialect_escapes)
                           if isinstance(c, SEscape)
                           else c for c in collect]
                    orig = collect
                    next_dash = False
                    collect = []
                    coll_str = []
                    #print('wathrm:', hrm)
                    for v, o in zip(hrm, orig):
                        #print(v)
                        # FIXME if a char came from somewhere other than
                        # the elisp reader, we could make a mistake here
                        if isinstance(v, type) and issubclass(v, char.NeedDash):
                            # FIXME still need instances here to hold points ?
                            collect.append(v)
                            next_dash = True
                        elif next_dash and v != '-':
                            raise SyntaxError(f'lol {ast}')
                        elif next_dash:
                            next_dash = False
                        elif isinstance(v, str):
                            coll_str.append(v)
                        elif v == char.UnicodeName:
                            _se = SEscape('N')
                            _se._point_beg = o._point_beg
                            _se._point_end = o._point_end
                            coll_str.append(_se)
                        elif coll_str:
                            # once you are in the final char spec
                            # you can't go back out
                            raise SyntaxError(f'oops {ast}')
                        else:
                            collect.append(v)

                    regular_char = False
                    if coll_str:  # TODO else error ??
                        if not collect:
                            regular_char = True
                        #print(coll_str)
                        if isinstance(coll_str[0], SEscape):
                            _ecs = ECharSpec(coll_str)
                            _ecs._point_beg = coll_str[0]._point_beg
                            _ecs._point_end = coll_str[0]._point_end
                            # FIXME still broken I think
                            collect.append(self.echarspec(_ecs).value)
                        else:
                            collect.append(self.charspec(CharSpec(coll_str)).value)

                    if regular_char:
                        nv = collect[0]
                    else:
                        wlk = CharEl(collect)
                        self._loc(wlk, ast)
                        return wlk

            elif isinstance(v0, str):
                vrest = ''.join(rest)  # FIXME TODO fails here -> syntax error
                # FIXME ?\^\n != ?\^n and is a valid value ;_; tears
                if v0 == 'x' and len(vrest) <= 7:
                    # ?\xfffffff is max and may have variable width
                    # ?\x seems to read as ?\x0 ?? shouldn't that signal an error ??
                    # NOTE python tops out at 0x110000 which is 1114111 which is the
                    # current max unicode char emacs just goes wild apparently
                    i = int(vrest, base=16)
                    try:
                        nv = chr(i)
                    except ValueError:
                        wlk = CharEl(None, elc_int=i)
                        self._loc(wlk, ast)
                        return wlk

                if v0 == 'u' and len(vrest) == 4:
                    # ?\uffff is max and fixed width
                    nv = chr(int(vrest, base=16))
                elif v0 == 'U' and len(vrest) == 8:
                    # elisp U must be 8 hex
                    # ?\U0010ffff seems to be max for the extended unicode
                    # but is not actually a valid unicode char right now
                    # python matches the error behavior here
                    nv = chr(int(vrest, base=16))
                else:
                    value = v0 + vrest
                    if not vrest:
                        nv = value
                    elif v0 in '01234567' and len(value) <= 3:
                        # emacs supports up to 777 aka 511 aka 512 or 9 bits
                        # but unlike racket does not require leading zeros
                        i = int(value, base=8)
                        if i > 511:
                            msg = ('Elisp does not support octal notation '
                                   'for chars longer than 9 bits. '
                                   'Aka 511, 0o777 or ?\\777.')
                            raise SyntaxError(msg)

                        nv = chr(i)

            else:
                raise SyntaxError(f'unknown char spec {ast}')
        else:
            # someone is up to some weirdness and this probably
            # isn't a real char spec
            raise SyntaxError(f'wat char spec {ast}')
            
        wlk = Char(nv)
        self._loc(wlk, ast)
        return wlk

        #return ast.value(lambda v:v, self._compute_escape_value)


class WalkUnion(Walk):
    """ All together now! """
