import os
import re
import sys
from pathlib import Path
import pytest
from sxpyr._exports import *
from sxpyr.walks import WalkRkt, WalkCl, WalkEl
from sxpyr.sxpyr import (
    conf_read, Walk, conf_plist, WalkPl, plist_to_dict, PList, Ast,
    make_do_path,
    UnknownStateError, DispatchNotImplementedError)

git_path = Path('~/git/').expanduser()
git_nofork_path = git_path / 'NOFORK'
test_data = Path(__file__).parent / 'data'

SKIP_CI = 'CI' in os.environ
skipif_ci = pytest.mark.skipif(SKIP_CI, reason='Need git paths and os paths.')
SKIP_NETWORK = ('SKIP_NETWORK' in os.environ or
                'FEATURES' in os.environ and 'network-sandbox' in os.environ['FEATURES'])
skipif_no_net = pytest.mark.skipif(SKIP_NETWORK, reason='Skipping due to network requirement')
SKIP_PATHS = not git_nofork_path.exists()
skipif_no_paths = pytest.mark.skipif(SKIP_CI or SKIP_NETWORK or SKIP_PATHS, reason='test files not present')

parse_common = configure()
parse_plist = configure(**conf_plist)
parse_sxpyr = configure(**conf_sxpyr)
parse_union = configure(**conf_union)
parse_cl = configure(**conf_cl)
parse_el = configure(**conf_el)
parse_xel = configure(**conf_xel)
parse_rkt = configure(**conf_rkt)
parse_gui = configure(**conf_gui)
parse_clj = configure(**conf_clj)
parse_hy = configure(**conf_hy)
parse_txr = configure(**conf_txr)


def sprint(gen, match=None, fail=False):
    res = []
    try:
        for parsed_expr in gen:
            if printexp:
                print('expr:', parsed_expr)

            res.append(parsed_expr)

        if fail:
            assert False, 'should have failed'
    except SyntaxError as e:
        if not fail:
            raise e
        else:
            print(f'task failed succssfully for ??? with {e}')
            return

    #print(res)
    if match is not None:
        assert res == match, (res, match)

    return res


def test_read():
    read_rkt = conf_read(parse_rkt, WalkRkt)
    read_el = conf_read(parse_el, WalkEl)

    # the dangers of recursion
    recursion_limit = sys.getrecursionlimit()
    rec_under = int(recursion_limit // 4.5)
    rec_over = int(recursion_limit // 4)
    print('under over on stack', rec_under, rec_over)
    # close enough to the limit that certain other things like calling
    # repr will hit the limit, however on versions of python newer than 3.6
    # it seems that repr no longer has this behavior (strange)
    big = next(read_rkt(''.join([p * rec_under for p in ("(", ")")])))
    try:
        repr(big)
        #assert False, "should have blown the stack"
    except RecursionError as e:
        pass

    try:
        oops = next(read_rkt(''.join([p * rec_over for p in ("(", ")")])))
        #assert False, "should have blown the stack"  # doesn't blow the stack in pypy3
    except RecursionError as e:
        pass

    # elisp chars
    a = next(read_el(r'?\^\M-\N{colon}'))
    b = next(read_el(r'?\^\M-:'))
    c = next(read_el(r'?\C-\M-:'))
    d = next(read_el(r'?\C-\M-\N{U+003a}'))
    assert a == b == c == d, f'{a} {b} {c} {d}'
    assert a.elc_int == 201326650, f'{a.elc_int} != 201326650'

    c1 = next(read_el(r'?\N{U+003A}'))
    c2 = next(read_el('?:'))
    assert c1 == c2, '{c1} != {c2}'

    # quasi-things and un-things
    sprint(read_rkt("#`(a #,b)"))
    sprint(read_rkt("#`(a #,@b)"))

    sprint(read_rkt("#'(a #,b)"))  # unq/s is allowed in quote or syntax
    sprint(read_rkt("#'(a #,@b)"))

    sprint(read_rkt("`(a #,b)"))
    sprint(read_rkt("`(a #,@b)"))

    sprint(read_rkt("#`(a ,b)"))
    sprint(read_rkt("#`(a ,@b)"))

    sprint(read_rkt("`(a ,@(b))"))
    sprint(read_rkt("`(a ,@(b `(c ,(d))))"))
    sprint(read_rkt("`(a ,@(b `(c ,(d `(e ,@(f))))))"))
    sprint(read_rkt("'(a ,@(b))"))
    sprint(read_rkt("`(a ,@(b ,(c)))"), fail=True)

    sprint(read_rkt("`(a ,(b))"))
    sprint(read_rkt("`(a ,(b `(c ,(d))))"))
    sprint(read_rkt("`(a ,(b `(c ,(d `(e ,(f))))))"))
    sprint(read_rkt("'(a ,(b))"))
    sprint(read_rkt("`(a ,(b ,(c)))"), fail=True)

    sprint(read_rkt("#`(a #,@(b))"))
    sprint(read_rkt("#`(a #,@(b #`(c #,(d))))"))
    sprint(read_rkt("#`(a #,@(b #`(c #,(d #`(e #,@(f))))))"))
    sprint(read_rkt("#'(a #,@(b))"))
    sprint(read_rkt("#`(a #,@(b #,(c)))"), fail=True)

    sprint(read_rkt("#`(a #,(b))"))
    sprint(read_rkt("#`(a #,(b #`(c #,(d))))"))
    sprint(read_rkt("#`(a #,(b #`(c #,(d #`(e #,(f))))))"))
    sprint(read_rkt("#'(a #,(b))"))
    sprint(read_rkt("#`(a #,(b #,(c)))"), fail=True)


@skipif_no_paths
def test_read_paths():
    def make_pr(parse, walk_cls):
        read = conf_read(parse, walk_cls)
        parse_path = make_do_path(parse)
        read_path = make_do_path(read)
        return read, parse_path, read_path

    def dol(pr, path):
        _, p, r = pr
        try:
            return list(p(path)), list(r(path))
        except RecursionError:
            pass
        except Exception as e:
            raise e

        raise RecursionError(f'exception in {path} {e}') #from e


    pr_common = read, parse_path, read_path = make_pr(parse_common, Walk)

    pr_pl = read_pl, parse_path_pl, read_path_pl = make_pr(parse_plist, WalkPl)  # FIXME WalkMax?

    pr_rkt = read_rkt, parse_path_rkt, read_path_rkt = make_pr(parse_rkt, WalkRkt)
    pr_el = read_el, parse_path_el, read_path_el = make_pr(parse_el, WalkEl)
    pr_cl = read_cl, parse_path_cl, read_path_cl = make_pr(parse_cl, WalkCl)

    pe1 = git_nofork_path / 'org-mode/lisp/ob-core.el'
    pe2 = git_nofork_path / 'emacs/test/src/syntax-tests.el'
    pe3 = git_nofork_path / 'emacs/lisp/simple.el' # anything but
    pr1 = git_nofork_path / 'racket/racket/collects/compiler/embed.rkt'
    pr2 = git_nofork_path / 'racket/racket/src/cs/schemified/expander.scm'  # big p

    p_rkt1, r_rkt1 = dol(pr_rkt, pr1)

    p_el1, r_el1 = dol(pr_el, pe1)
    p_el2, r_el2 = dol(pr_el, pe2)
    p_el3, r_el3 = dol(pr_el, pe3)

    p_rkt1, p_el1, p_el2, p_el3 = None, None, None, None  # debug repr slowness

    # test plist with caste to python
    ps1 = test_data / 'plist-test.sxpr'
    # (apply hash-map '(:a b :c d))
    # but need recursive
    # (apply hash-map '(:a b :c (:d e :f g)))
    p_1, r_1 = dol(pr_pl, ps1)
    def cf(ast):
        if isinstance(ast, PList):
            return plist_to_dict(ast.value)
        elif isinstance(ast, Ast):
            return ast.value
        else:
            return ast

    c_1 = [_.caste(cf) for _ in r_1]


def test_parse(debug=False):
    # asdf

    # racket wat
    sprint(parse_rkt("""
before
#|
        a
       /|\
      b c e
        |
        d
|#
after
    """))

    # hy
    # sprint(parse_hy("""#[[<{} object; :span (, 4 7) :match "bbb">]]"""))  # XXX TODO haven't implemented this

    # xemacs comma in comment in quasiquote
    sprint(parse_xel('`(a b c " d e, " f g\n)'))
    sprint(parse_xel('`(a b c ;; d e, f g\n)'))

    # plist sharp weirdness FIXME pretty sure that we don't want this
    # behavior where sharp eats until the next expression, or rather
    # it should eat, but internally Sharp ListP should be an error?
    sprint(parse_plist("""(
    # ; comment
    (sharpified thing))"""))

    # xelisp symbols and unquote in quasi
    sprint(parse_xel("`(a ,b,c)"))  # FIXME OH NO is it (unquote b) (unquote c) or what?!
    # in xemacs this reads with b,c as a single symbol in gemacs, it very much does not
    # and this is why you don't allow context sensitive things in the input layer

    sprint(parse_xel("`(a,b)"))
    # XXX so apparenly xemacs actually doesn't unquote b but instead
    # reads a,b as a symbol, gemacs however does what I would expect,
    # and I would classify the behavior here as a bug and require the
    # use of a\,b as in gemacs
    sprint(parse_xel("(`a,b)"))  # man there is a lot of weirdness in elisp

    # runnin (read "(list 1 , 2 , 3)") in gemacs well the explains a lot

    sprint(parse_xel("`(a ,b)"))
    sprint(parse_xel(r"`(a\,b)"))
    sprint(parse_xel("`(a ,(b `(c ,d)))"))
    sprint(parse_xel("`(1 ,(a,b `(3 ,c,d)))"))

    sprint(parse_xel("(',)"))
    sprint(parse_xel("`(,a)"))

    sprint(parse_xel("`,a"))
    sprint(parse_xel("`,\na"))
    # sprint(parse_xel("`,")) # FIXME this reads as nothing if using
    # C-x C-e however the form presented here does cause EOF errors in
    # nearly every dialect

    sprint(parse_xel('sigh-,-variants'), match=[Atom('sigh-,-variants')])

    # wat
    hrm = 'oh;no'
    sprint(parse_xel(hrm), match=[Atom('oh'), Comment('no')])
    hc_not_in_symbol = 'oh#*no'
    sprint(parse_sxpyr(hc_not_in_symbol), match=[Atom('oh#*no')])
    uok = '(;)\n)'
    sprint(parse_xel(uok), match=[ListP.from_elements(Comment(')'))])
    urg = '(x;)\n)'
    sprint(parse_xel(urg), match=[ListP.from_elements(Atom('x'), Comment(')'))])

    sprint(parse_cl('#"!"'))

    # FIXME cl and clojure double colon and beyond
    sprint(parse_cl('::k'))
    sprint(parse_cl(':::k'))
    sprint(parse_clj('::k'))
    sprint(parse_clj(':::k'))

    # FIXME racket ambiguous reading issues
    sprint(parse_rkt('#rx"lol"'))
    sprint(parse_rkt('#rx "lol"'))
    sprint(parse_rkt('#hash(a b c d)'))

    sprint(parse_rkt(""))
    sprint(parse_rkt("#{HRM}#"))  # FIXME ambiguous, used by guile
    sprint(parse_rkt("#{HRM} #"))  # with this

    sprint(parse_rkt("(# \"lol\")"))
    sprint(parse_rkt("#"))
    sprint(parse_rkt("##"))

    sprint(parse_rkt("'#:|fd\\sA|"))

    sprint(parse_clj("({'({:really? 'yes}) :wat 'thing :data} '({:really? 'yes}))"))
    sprint(parse_clj("({[{:really? 'yes}] :wat 'thing :data} [{:really? 'yes}])"))

    # these should read but will error later
    # because they have no label
    sprint(parse_rkt("#="))
    sprint(parse_rkt("##"))
    sprint(parse_rkt("#0=(1 2 3 . #0#)"))
    #sprint(parse_rkt("('##)"))  # TODO

    sprint(parse_rkt("#:|a b|"))
    # atom vs keyword distinction
    sprint(parse_rkt(":|a b|"))
    sprint(parse_rkt("|:a b|"))

    sprint(parse_rkt(":|d / e|"))
    sprint(parse_rkt("|:f / g|"))

    sprint(parse_rkt(r'#| #\; |#'))

    sprint(parse_rkt('#\\" lol'), match=[CharSpec('"'), Atom('lol')])
    sprint(parse_rkt('(#\\))'))
    sprint(parse_rkt(r'"."'))
    sprint(parse_rkt(r'#\0'))
    sprint(parse_rkt(r'#\.'))
    sprint(parse_rkt(r'#\space'))

    sprint(parse_rkt(r'"\""'))

    sprint(parse_rkt(r"(#\0)"))
    sprint(parse_rkt(r"#\0"))
    sprint(parse_rkt("\\#"))
    sprint(parse_rkt("'\\#"))

    # FIXME this means we have to preserve the escapes in atoms too!!!!
    sprint(parse_rkt('\1e+NaN')) # -> symbol
    sprint(parse_rkt('1e+NaN'))  # -> number

    # sharp things

    #sprint(parse_rkt("#"))
    sprint(parse_rkt("##HRM"))
    sprint(parse_rkt("#;1 2"))
    sprint(parse_rkt("#; 3 4"))
    sprint(parse_rkt("#; hello"), match=[XComment(Atom('hello'))])
    sprint(parse_hy("#_ there"), match=[XComment(Atom('there'))])
    sprint(parse_rkt("#a"))
    sprint(parse_rkt("#hash"))
    sprint(parse_rkt("#\\z"))
    sprint(parse_rkt("#'a"))
    sprint(parse_rkt("#`a"))
    sprint(parse_rkt("#,a"), match=[Sharp(UQuote(Atom('a')))])
    sprint(parse_rkt("#,@a"), match=[Sharp(SUQuote(Atom('a')))])

    # splicing unquote
    sprint(parse_rkt(r"'(regular , @ oops)"))
    sprint(parse_rkt(r"'(regular , @unquote)"))
    sprint(parse_rkt(r"'(splicing ,@unquote)"))
    sprint(parse_rkt(r"'(splicing ,@ unquote2)"))

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

    # charlits old
    sprint(parse_rkt(r'(list #\ a)'))
    sprint(parse_rkt(r'(list #\,a)'))
    sprint(parse_rkt(r'(list #\`a)'))
    sprint(parse_rkt(r'(list #\|a)'))

    sprint(parse_rkt(r'(list ?\ a)'))
    sprint(parse_rkt(r'(list ?\,a)'))
    sprint(parse_rkt(r'(list ?\`a)'))
    sprint(parse_rkt(r'(list ?\|a)'))

    sprint(parse_clj(r'(list \a\b\c)'))
    sprint(parse_clj(r'(list \a\b\c)'))
    # FIXME ?asdf and \asdf both cause read errors

    sprint(parse_rkt(r'(modify-syntax-entry ?\{  "(}1nb" table)'))
    sprint(parse_el(r'(modify-syntax-entry ?\{  "(}1nb" table)'))
    sprint(parse_el("?a?b?c"))
    sprint(parse_el("(list ?a?b?c)"))
    sprint(parse_el("""?
"""))

    sprint(parse_el("? "))
    sprint(parse_el("?)"))
    # wow, so this actually works, it is just hard to get
    # it to evaluate must be a legacy thing
    sprint(parse_el("?("))

    sprint(parse_rkt("#|| lol |||||#"))
    sprint(parse_rkt("a #| b #| c |# d |# e"))

    sprint(parse_rkt("(|Append| '|Append|)"))
    sprint(parse_cl("(`())"))
    #sprint(parse_cl("(a (b (c)) (d (e) `(f (g) h) ((i) j)))"))
    #sprint(parse_rkt("(a (b (c)) (d (e) `(f (g)  (1 (2)) (3 (4)) (ca 'q (* (f (1) (2))h))) ((i) j)))"))

    sprint(parse_rkt("a|b c|d"))
    sprint(parse_rkt("|a|b "))
    sprint(parse_rkt("|a| b"))

    # welcome to the watverse
    sprint(parse_rkt("(and 'a'b'c)"))

    sprint(parse_clj(":see-alsos [:clojure.core/* :clojure.core/*'],"))  # clojure weirdness  TODO

    # O ... K ... today we learned something about python strings
    # which brings us back to the question of why our example was failing
    sprint(parse_rkt(r";\
    "))
    sprint(parse_rkt(r";\
I can't believe you've done this."))
    sprint(parse_rkt(
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
    sprint(parse_rkt('(#\\))'))
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
    sprint(parse_rkt(r'#\['))
    sprint(parse_rkt('#| | |#'))

    sprint(parse_rkt(r'"\\"'))
    sprint(parse_rkt(r'("\\")'))
    sprint(parse_rkt(r'[("\\")]'))

    sprint(parse_rkt("(define lol #f)"))
    sprint(parse_rkt("[define hah #t]"))

    sprint(parse_rkt("{:a 1 :b 2 :c 3}"))
    sprint(parse_rkt("{:a 1 :b {hello there} :c 3}"))

    sprint(parse_rkt("#'stx"))
    sprint(parse_rkt("#`stx"))

    # feature expressions
    sprint(parse_rkt("#+\n()"))
    sprint(parse_rkt("#-\n()"))
    sprint(parse_rkt("#- (not feature)"))

    sprint(parse_rkt("#+()"))
    sprint(parse_rkt("#-()"))
    sprint(parse_rkt("#-(and yes I have features)"))
    sprint(parse_rkt("#-nil"))
    sprint(parse_rkt("#+nil"))
    sprint(parse_rkt("#+nil #-nil #-nil #-nil 'a"))
    sprint(parse_rkt("#+(or oh no) woop"))

    # eXpression comments
    sprint(parse_rkt("#;\n(a #; b c)"))
    sprint(parse_rkt("(a #; b c)"))
    sprint(parse_rkt("(a #_ b c)"))
    sprint(parse_union("(a #_#; b c)"))
    sprint(parse_rkt("(a #; #; b c)"))
    sprint(parse_union("(a #_ #_ b c)"))

    sprint(parse_rkt("' ; shi\nt"))

    sprint(parse_rkt('(a"b\\"c"d)'), match=[
        ListP.from_elements(
            Atom('a'),
            EString(['b', SEscape('"'), 'c']),
            Atom('d'),)
    ])  # should fail

    sprint(parse_rkt("('())"), match=[
        ListP.from_elements(Quote(ListP.from_elements()),)
    ])

    sprint(parse_rkt("('()) a"), match=[
        ListP.from_elements(Quote(ListP.from_elements()),),
        Atom('a')
    ])

    sprint(parse_rkt("(X\"\")"), match=[
        ListP.from_elements(
            Atom('X'),
            '')
    ])
    sprint(parse_rkt("'(1'q)"), match=[
        Quote(
            ListP.from_elements(
                Atom('1'),
                Quote(Atom('q'))))
    ])
    sprint(parse_rkt("'(('q)a)"), match=[
        Quote(
            ListP.from_elements(
                ListP.from_elements(
                    Quote(Atom('q')),),
                Atom('a')))
    ])

    sprint(parse_rkt("('(x))"))
    sprint(parse_rkt("()"))
    sprint(parse_rkt("(())"))
    sprint(parse_rkt("(()())"))
    sprint(parse_rkt("'(1)"))
    sprint(parse_rkt("'(1())"))
    sprint(parse_rkt("'(('q))"))
    sprint(parse_rkt("''''''''''q"))

    # quasiquote
    sprint(parse_rkt("(`(x))"))
    sprint(parse_rkt("`(1)"))
    sprint(parse_rkt("`(1())"))
    sprint(parse_rkt("`((`q))"))
    sprint(parse_rkt("``````````q"))
    sprint(parse_rkt("`(1(2)`(3)(`4)`(`5))"))

    sprint(parse_rkt("(a (b)) c"))
    sprint(parse_rkt("'(1(2)'(3)) "))
    sprint(parse_rkt("'(1(2)'(3))"))

    sprint(parse_rkt("'(1(2))"))
    sprint(parse_rkt("'(1(2)'3)"))
    sprint(parse_rkt("'(1(2)'(3)('4))"))
    sprint(parse_rkt("'(1(2)'(3)) oops"))
    sprint(parse_rkt("'(1(2)'(3)('4)f)"))
    sprint(parse_rkt("'(1(2)'(3)('4)())"))
    sprint(parse_rkt("'(1(2)'(3)('4)'())"))
    sprint(parse_rkt("'(1(2)'(3)('4)'('5))"))
    sprint(parse_rkt('(a (b))\nc'))
    sprint(parse_rkt('(d ; comment\n(e))\nf'))
    sprint(parse_rkt(';comment'))
    sprint(parse_rkt('(symbol :keyword 1234 "string")'))
    sprint(parse_rkt('symbol'))
    sprint(parse_rkt('1234'))
    sprint(parse_rkt('"string"'))
    sprint(parse_rkt('t'))
    sprint(parse_rkt('#t'))
    sprint(parse_rkt('#f'))
    sprint(parse_rkt("'()"))
    sprint(parse_rkt("'quoted-symbol"))
    sprint(parse_rkt("       leading whitespace"))
    sprint(parse_rkt("trailing whitespace               "))
    sprint(parse_rkt("   'leading"))
    sprint(parse_rkt("'trailing    "))
    sprint(parse_rkt("'(((((((((((())))))))))))"))
    sprint(parse_rkt("wat 'trailing"))
    sprint(parse_rkt("symbol'quote more"))
    sprint(parse_rkt(":keyword'quote"))
    sprint(parse_rkt("''quote"))
    sprint(parse_rkt("'quote'quote"))
    sprint(parse_rkt("'quote(sym1)"))
    sprint(parse_rkt("'quote[sym2]"))
    sprint(parse_rkt("sym(sym3)"))
    sprint(parse_rkt("sym[sym4]"))
    sprint(parse_rkt("1[sym5]"))
    sprint(parse_rkt("2[sym6]"))
    sprint(parse_rkt("(a (b))"))
    sprint(parse_rkt("'(a (b))"))
    sprint(parse_rkt("'(1 2 3 4 (asdf asdf))"))
    sprint(parse_rkt("'(5 6 7 8 (asdf asdf))\n(desire to know more intensifies)"))
    sprint(parse_rkt("(defun asdf (there) 'hello there)"))
    sprint(parse_rkt("I am multiple atoms that should be read in a row. (indeed) \"yes\""))
    sprint(parse_rkt('(testing "I am a happy string \\" yay!" string escape)'))
    # NOTE if you have unmatched parens in a here string that means that
    # this parser will fail
    sprint(parse_rkt("""
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
    sprint(parse_rkt(test_cfg1))


def test_chars(debug=False):
    # charachter literals
    def make_test_char(char_marker):
        def inner (s):
            if isinstance(s, str):
                return '(' + char_marker + s + ')'
            else:
                return '(' + ''.join([char_marker + ss for ss in s]) + ')'

        return inner

    test_suffixes = (
              ("\\", ListP.from_elements(CharSpec('\\'),)),
              (" ", ListP.from_elements(CharSpec(' '),)),
              (";", ListP.from_elements(CharSpec(';'),)),
              (")", ListP.from_elements(CharSpec(')'),)),
              ("z", ListP.from_elements(CharSpec('z'),)),  # can't use a because emacs -> x07
              ("  a", ListP.from_elements(CharSpec(' '), Atom('a'))),
              ("  'a", ListP.from_elements(CharSpec(' '), Quote(Atom('a')))),
              (") a", ListP.from_elements(CharSpec(')'), Atom('a'))),
              (" 'a", ListP.from_elements(CharSpec(' '), Quote(Atom('a')))),
              (")'a", ListP.from_elements(CharSpec(')'), Quote(Atom('a')))),
              (" a", ListP.from_elements(CharSpec(' '), Atom('a'))),
              (";a", ListP.from_elements(CharSpec(';'), Atom('a'))),
              (")a", ListP.from_elements(CharSpec(')'), Atom('a'))),
              ("'a", ListP.from_elements(CharSpec("'"), Atom('a'))),
              ("aa", ListP.from_elements(CharSpec('aa'),)),  # XXX REMINDER that non-existent char errors are NO LONGER reader errors
              (("y", "z"), ListP.from_elements(CharSpec('y'), CharSpec('z'))),
              ("newline", ListP.from_elements(CharSpec('newline'),)),
              #("SpAcE", ListP.from_elements(CharSpec('space'),)),  # TODO this only works after walk
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
                parser = parse_rkt

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
                    ast = ast.from_elements(*(ECharSpec([SEscape(_.value)])
                                              if isinstance(_, CharSpec)
                                              else _ for _ in ast.collect))

                # a number of these now read without error and the
                # fact that there is no such charachter is deferred
                if string == ' a':
                    ast = ListP.from_elements(ECharSpec([SEscape(' '), 'a']),)
                elif string == ';a':
                    ast = ListP.from_elements(ECharSpec([SEscape(';'), 'a']),)
                elif string == ')a':
                    ast = ListP.from_elements(ECharSpec([SEscape(')'), 'a']),)
                elif string == '\'a':
                    ast = ListP.from_elements(ECharSpec([SEscape('\''), 'a']),)
                elif string == 'aa':
                    ast = ListP.from_elements(ECharSpec([SEscape('a'), 'a']),)
                elif string == 'newline':
                    ast = ListP.from_elements(ECharSpec([SEscape('n'), *'ewline']),)

                sprint(parser(tstr), match=[ast])
            else:
                sprint(parser(tstr), match=[ast])


@skipif_no_paths
def test_parse_paths():
    paths = (
        #(git_nofork_path / 'racket/racket/share/pkgs/future-visualizer/future-visualizer/tests/visualizer.rkt'),
        (git_path / 'protc/anno-tags.rkt'),
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
        (git_nofork_path / 'hy/tests/native_tests/hy_repr.hy'),  # broken due to #[strings]

        Path('/usr/lib/xemacs/xemacs-packages/lisp/edebug/edebug.el'),
        Path('/usr/lib/xemacs/xemacs-packages/lisp/tm/latex-math-symbol.el'),
        Path('/usr/lib/xemacs/xemacs-packages/lisp/footnote/footnote-kana.el'),
        Path('/usr/lib/xemacs/xemacs-packages/lisp/haskell-mode/haskell-decl-scan.el'),
        Path('/usr/lib/xemacs/xemacs-packages/lisp/psgml/psgml-dtd.el'),

        #)
        #_sigh = (

        # txr
        *(git_nofork_path / 'txr').rglob('*.tl'),

        # fennel
        *(git_nofork_path / 'Fennel').rglob('*.fnl'),

        # edn
        #test_data / 'export.edn',
        *(git_nofork_path / 'edn-dot-net').rglob('*.edn'),

        # clj
        *(git_nofork_path / 'clojure').rglob('*.clj'),
        *(git_nofork_path / 'spec.alpha').rglob('*.clj'),
        *(git_nofork_path / 'core.specs.alpha').rglob('*.clj'),
        *(git_nofork_path / 'tawny-owl').rglob('*.clj'),

        # hy
        *Path('/usr/lib/python3.7/site-packages/hy/contrib').rglob('*.hy'),
        *(git_nofork_path / 'hy').rglob('*.hy'),

        # racket
        *(git_path / 'protc/protc-lib/protc').rglob('*.rkt'),
        *(git_nofork_path / 'racket').rglob('*.rkt'),  # wow ... fast

        # chez
        *(git_nofork_path / 'racket').rglob('*.scm'),

        # guile
        *Path('/usr/share/guile/2.2/').rglob('*.scm'),

        # gambit (wow that install location)
        *Path('/usr/lib64/').rglob('*.scm'),

        # common lisp
        *(git_nofork_path / 'sbcl').rglob('*.lisp'),
        *(git_nofork_path / 'ccl').rglob('*.lisp'),

        # elisp
        *(git_nofork_path / 'emacs/').rglob('*.el'),  # elisp uses ? for chars
        *(git_nofork_path / 'org-mode/').rglob('*.el'),  # elisp uses ? for chars
        # https://yoo2080.wordpress.com/2013/11/25/question-mark-and-emacs-lisp/

        # xemacs elisp
        *Path('/usr/lib/xemacs').rglob('*.el'),

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
        'pat-unify.rkt',   # #lang 2d issues
        'chat-noir-literate.rkt',  # #lang scribble/lp
        'std-grammar.rkt', # sciribble issues
        #'scribble/reader.rkt',  # here string

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

        # xelisp
        'tex-jp.el',        # coding:iso-2022-jp-unix
        'mew-lang-jp.el',   # unmarked coding:iso-2022-7bit-unix emacs uses hueristics
        'riece-mcat-japanese.el',  # coding: iso-2022-jp
        'footnote-kana.el',        # coding system issues
        'latex-math-symbol.el',    # unmarked coding:iso-2022-7bit-unix emacs uses hueristics

        # clojure
        #'reducers.clj',    # *' issue
        #'transducers.clj', # end with ' issue ... need to look into this ... very rare to see but not banned
        #'memorise_test.clj',  # #{ issue
        #'owl.clj',         # end with '

        # hy
        'hy_repr.hy',  # #[string] issues
        'compiler_error.hy',  # expected unterminated string

        # fennel
        *(_.name for _ in (git_nofork_path / 'Fennel/test/bad/').rglob('*.fnl')),

        # txr
        'ffi-misc.tl',  # #b'string' issues?
        'split.tl', # #/regex/ issues?
        'termios.tl',  # `str\ning` issues
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

            elif hrm.startswith('#lang scribble'):
                continue

            try:
                if p.suffix == '.tl':
                    tres = sprint(parse_txr(hrm))

                res = sprint(parse_rkt(hrm))  # racket reader compatible syntax
                print('success:', p)
                #val = list(parse_rkt(hrm))
            except Exception as err:
                try:
                    if p.name in should_fail:
                        continue
                    elif p.suffix == '.tl':
                        if re.search(r'`(?!`).*`', hrm):
                            print('txr fail: `string`')
                            continue
                        elif '#b' in hrm:
                            print('txr fail: #b binary notation')
                            continue
                        else:
                            raise err
                    elif p.suffix == '.rkt':
                        if '#<<' in hrm:
                            print('racket fail: here string', p)
                            # here string
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
                        try:
                            res = sprint(parse_el(hrm))
                            print('success el:', p)
                        except Exception as err3:
                            if 'xemacs' in p.as_posix():
                                print('fail el:', p)
                                res = sprint(parse_xel(hrm))
                                print('success xel:', p)
                            else:
                                raise err3 from err
                            #if '?' in hrm:
                                #print('fail ? issue:', p, err2)
                            #else:
                                #raise err2
                        continue
                    elif p.suffix in ('.clj', '.edn'):
                        print('generic failed:', p, err)
                        res = sprint(parse_clj(hrm))
                        print('success clj:', p)
                        continue
                    elif p.suffix == '.scm' and 'guile' in p.as_posix():
                        print('generic failed:', p, err)
                        res = sprint(parse_gui(hrm))
                        print('success gui:', p)
                        continue
                    elif p.suffix == '.hy':
                        print('generic failed:', p, err)
                        res = sprint(parse_hy(hrm))
                        print('success hy:', p)
                        continue

                except Exception as err2:
                    raise err2.__class__(p) from err2

                debug = True
                try:
                    sprint(parse_rkt(hrm))
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


def test_fails_read():
    read_rkt = conf_read(parse_rkt, WalkRkt)
    bads = ('#',
            '# ',
            '##',
            '#_',
            "#+ ()",
            # #\\\x1a is ok #\^Z -> #\u001A but not #^Z
            # this should fail in read but not parse
            # it triggers bad syntax in racket
            '#\x1a',
            )
    run_fails(read_rkt, bads)


def test_fails_parse():
    bads = (
        ',',
        "(defun fail (parse)",
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
        "|a| b|",
        '#| #| |#',
        '(;hrm',  # comment state could mask unterminated list
        '(#;',
        '(#||#',
        '#;',
        '#\\',  # note #\ unterminated char in racket
            )
    run_fails(parse_rkt, bads)


def run_fails(read_or_parse, bads):
    should_have_failed = []
    for bad in bads:
        try:  # I WANT MACROS
            sprint(read_or_parse(bad))
            should_have_failed.append(bad)
        except (UnknownStateError, DispatchNotImplementedError) as e:
            print(f'task failed succssfully as {e!r} for {bad!r}')
        except SyntaxError as e:
            print(f'task failed succssfully for {bad!r}')
        except NotImplementedError as e:
            print(f'task failed succssfully as not implemented for {bad!r}')

    assert not should_have_failed, should_have_failed


if __name__  == '__main__':
    debug = '-d' in sys.argv
    printexp = debug or '-e' in sys.argv
    if debug:
        import sxpyr.sxpyr
        sxpyr.sxpyr.debug = True

    test_read()
    test_read_paths()
    #sys.exit()
    test_parse(debug=debug)
    test_parse_paths()
    test_fails_read()
    test_fails_parse()
    test_chars(debug=debug)

else:
    printexp = False
