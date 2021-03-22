import ast
import pathlib
from pyontutils.utils import asStr
here = pathlib.Path(__file__).parent

state_names_full = dict(
    bos= 'Top Level',
    s_atom= 'Atom',
    s_keyw= 'Keyword',
    s_quote= 'Quote',
    s_unquote= 'Unquote',
    s_unq_first= 'Unquote First',
    s_unq_splice= 'Unquote Splicing',
    s_quasiq= 'Quasiquote',
    s_string= 'String',
    s_list_p= 'List Paren',
    s_list_s= 'List Square',
    s_list_c= 'List Curly',
    s_sharp= 'Dispatch',  # dispatch
    s_comm= 'Comment',
    s_esc= 'Escape',
    s_esc_str= 'Escape String',
    s_comment_x= 'Comment Expression',
    s_comment_h= 'Comment Heading',  # no current dialects have this, inspired by org
    s_feat_x= 'Expression Feature',
    s_cblk= 'Comment Block',
    s_pi_in_cblk= 'Comment Block Maybe Exit',  # maybe leave block comment
    s_atom_verbatim= 'Atom Verbatim',
    s_comment_b_nest= 'Comment Block Nested',
    s_char= 'Charachter Literal',
    s_char_first= 'Charachter Literal First',
    )

token_names_abstract = dict(
    # common
    t_newline='Newline',
    t_tab='Tab',
    t_space='Space',
    t_beg_list_p='List Paren Begin',
    t_end_list_p='List Paren End',

    t_beg_end_str='String Delimiter',

    t_to_sharp='Dispatch',

    t_to_quote='Quote',
    t_to_quasi='Quasiquote',
    t_to_unquote='Unquote',

    t_to_splc_in_unq='Splice in Unquote',

    t_to_esc_in_str='Escape in String',

    t_to_comment='Comment',

    # uncommon
    t_beg_list_s='List Square Begin',
    t_end_list_s='List Square End',
    t_beg_list_c='List Curly Begin',
    t_end_list_c='List Curly End',

    t_beg_end_aver='Verbatim Atom Delimiter',

    t_to_cblk_in_shrp='Block Comment in Dispatch',
    t_to_xcom_in_shrp='Expression Comment in Dispatch',
    t_to_hcom_in_shrp='Heading Comment in Dispatch',  # XXX
    t_to_keyw_in_shrp='Keyword in Dispatch',
    t_to_feat_in_shrp='Feature Expression in Dispatch',
    t_to_strn_in_shrp='Expression Comment in Dispatch',  # TODO only used in hy, adds more states, not sure if want

    t_to_unquote_in_quasi='Unquote in Quasiquote',  # this is among the most evil of all

    t_to_keyw='Keyword',
    t_to_esc='Escape',

    t_to_char='Charachter',
)

def analysis():
    with open(here / 'sxpyr.py', 'rt') as f:
        tree = ast.parse(f.read())

    ast_conf = [n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == 'configure'][0]
    ast_state = [n for n in ast_conf.body if isinstance(n, ast.Assign) and n.targets[0].id == 'states'][0]

    conf_args = [arg.arg for arg in ast_conf.args.args]
    abstract_tokens = [t for t in conf_args if t.startswith('t_')]
    n_abstract_tokens = [token_names_abstract[t] for t in abstract_tokens]
    states = [n.id for n in ast_state.targets[-1].elts]
    n_states = [state_names_full[s] for s in states]
    return n_abstract_tokens, ['-------------'], n_states
