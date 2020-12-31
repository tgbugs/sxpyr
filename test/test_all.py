import sys
from sxpyr import syms_cl, syms_el, syms_gui, syms_clj, syms_hy
from sxpyr import configure
from sxpyr import *


def test():
    debug = '-d' in sys.argv
    printexp = debug or '-e' in sys.argv
    if debug:
        import sxpyr.sexp
        sxpyr.sexp.debug = True

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

    sprint(parse("(# \"lol\")"))
    sprint(parse("#"))

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
            EString(['b', SEscape('"'), 'c']),
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

             # fennel
             *(git_nofork_path / 'Fennel').rglob('*.fnl'),

             # edn
             Path('export.edn'),

             # clj
             *(git_nofork_path / 'clojure').rglob('*.clj'),
             *(git_nofork_path / 'spec.alpha').rglob('*.clj'),
             *(git_nofork_path / 'core.specs.alpha').rglob('*.clj'),
             *(git_nofork_path / 'tawny-owl').rglob('*.clj'),

             # common lisp
             *(git_nofork_path / 'sbcl').rglob('*.lisp'),

             # hy
             *Path('/usr/lib/python3.7/site-packages/hy/contrib').rglob('*.hy'),
             *(git_nofork_path / 'hy').rglob('*.hy'),

             # chez
             *(git_nofork_path / 'racket').rglob('*.scm'),

             # racket
             *(git_path / 'protc/protc-lib/protc').rglob('*.rkt'),
             *(git_nofork_path / 'racket').rglob('*.rkt'),  # wow ... fast

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

        # fennel
        *(_.name for _ in (git_nofork_path / 'Fennel/test/bad/').rglob('*.fnl'))
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


def test_fails():
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
