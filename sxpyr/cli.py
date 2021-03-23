""" sxpyr parse lisp

Usage:
    sxpyr parse [options] <path>...

Options:
    -d --debug
"""

import pathlib
from pyontutils import clifun as clif
from sxpyr import sxpyr as sxpyrmod
from sxpyr import (conf_sxpyr,
                   conf_cl,
                   conf_el,
                   conf_xel,
                   conf_rkt,
                   conf_gui,
                   conf_clj,
                   conf_hy)
from sxpyr.walks import WalkRkt, WalkCl, WalkEl
from sxpyr import configure
from sxpyr.sxpyr import make_do_path
from sxpyr.sxpyr import conf_read, Walk, conf_plist, WalkPl, plist_to_dict, PList, Ast
from sxpyr import *



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

read_rkt = conf_read(parse_rkt, WalkRkt)
read_el = conf_read(parse_el, WalkEl)


class Options(clif.Options):

    @property
    def path(self):
        return [pathlib.Path(path).expanduser() for path in self._args['<path>']]


class Main(clif.Dispatcher):

    def default(self):
        raise NotImplementedError('oops')

    def parse(self):
        # TODO guess dialect
        sxpyrmod.debug = self.options.debug  # FIXME sigh naming imports etc
        parse = parse_rkt  # FIXME
        walk = WalkRkt()
        read = read_rkt  # FIXME
        def walk_ast(ast_it):
            # ah if only you could have names land like waterwheels on
            # a river, at each point without having to tuple
            for expression in ast_it:
                if self.options.debug:
                    print('read:', expression)
                if not walk._skip(expression):
                    transformed = walk(expression)
                    yield transformed

        parse_path = make_do_path(parse)
        read_path = make_do_path(read)

        asts, wlks = [], []
        ast_fail, wlk_fail = [], []
        bad_walk  = []
        for path in self.options.path:
            try:
                ast_generator = parse_path(path)
                ast = list(ast_generator)
                asts.append(ast)

                wlk_generator = (walk_ast(ast))
                wlk = list(wlk_generator)
                wlks.append(wlk)
                repr(wlk)
            except AttributeError:
                breakpoint()
                raise
            except SyntaxError as e:
                if len(asts) > len(wlks):
                    bad_walk.append(asts.pop())
                    wlk_fail.append((path, e))
                else:
                    ast_fail.append((path, e))
                

        #print(asts)
        print(wlks)
        print(bad_walk)
        if wlk_fail:
            print('walk failures', wlk_fail)
        if ast_fail:
            print('ast failures', ast_fail)
        return asts, wlks


def main():
    options, *ad = Options.setup(__doc__, version='sxpyr 0.0.0')

    main = Main(options)

    if main.options.debug:
        print(main.options)

    out = main()


if __name__ == '__main__':
    main()
