# burn baby burn

from unicodedata import name as uniname, lookup as unilookup


class ELispCharThing:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f'<ELCT {self.value!r}>'

    def __hash__(self):
        return hash((self.__class__, self.value))

    def __eq__(self, other):
        return type(self) == type(other) and self.value == other.value


class KeyChar:
    """ Base for char escape things. """


class UnicodeName(KeyChar):
    """ Elisp madness. """


class NeedDash(KeyChar):
    """ Sigh. """


class MetaNeedDash(NeedDash):
    """ Alt to the rest of us. """
    bit = 2 ** 27


class HyperNeedDash(NeedDash):
    """ ??? to the rest of us. """
    bit = 2 ** 24


class AltNeedDash(NeedDash):
    """ Alt to the rest of us.
        Not sure about relation to Meta here. """
    bit = 2 ** 22


class ControlHat(KeyChar):
    """ Maybe this is cmd on macs? No idea. """
    bit = 2 ** 26


class ControlNeedDash(NeedDash):
    """ Maybe this is cmd on macs? No idea. """
    bit = 2 ** 26


class ShiftNeedDash(NeedDash):
    r""" ?\S- ?? No idea. """
    bit = 2 ** 25


class SpaceOrSuperNeedDash(NeedDash):
    r""" ?\s- ?? No idea. """
    bit = 2 ** 23


class Octal(KeyChar):
    r""" ?\000 to ?\777 """

    def __init__(self, integer):
        self.value = integer

    def __repr__(self):
        return f'Octal({self.value})'


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
    #**cee_oct,
    'd': '\x7f',  # delete aka rubout
    'e': '\x1b',  # escape
    's': SpaceOrSuperNeedDash,  # space
    #'s-': ????,  # FIXME AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    'C': ControlNeedDash,  # FIXME must be followed by a dash
    'M': MetaNeedDash,     # and emacs denotes this with a -1
    'S': ShiftNeedDash,
    'H': HyperNeedDash,
    'A': AltNeedDash,
    'N': UnicodeName,  # must be followed by {...}
    '^': ControlHat,  # emacs denotes this with a -1
}

cee_gui = {
    **cee_base,
    #**cee_oct,
}


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


def dialect_char(collect):
    class self: """ lol """

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
                return self.value
            except KeyError:
                pass

            raise ValueError(f'Unknown charachter name {v!r}')

        return self.value


