import re
from setuptools import setup


def find_version(filename):
    _version_re = re.compile(r"__version__ = ['\"](.*)['\"]")
    for line in open(filename):
        version_match = _version_re.match(line)
        if version_match:
            return version_match.group(1)


__version__ = find_version('sxpyr/__init__.py')

with open('README.org', 'rt') as f:
    long_description = f.read()

cli_requires = ['pyontutils']  # FIXME should be orthauth but haven't moved clifun yet
fuzz_requires = ['python-afl'] + cli_requires
tests_require = ['pytest'] + cli_requires
                 
setup(name='sxpyr',
      version=__version__,
      description='A flexible Lisp reader.',
      long_description=long_description,
      long_description_content_type='text/markdown',  # FIXME org -> markdown
      url='https://github.com/tgbugs/sxpyr',
      author='Tom Gillespie',
      author_email='tgbugs@gmail.com',
      license='MIT',
      classifiers=[
          'Development Status :: 3 - Alpha',
          'License :: OSI Approved :: MIT License',
          'Programming Language :: Python :: 3.7',
          'Programming Language :: Python :: 3.8',
          'Programming Language :: Python :: 3.9',
      ],
      keywords=('lisp reader edn clojure racket scheme '
                'elisp emacs sexp s-expression parser parsing'),
      packages=[
          'sxpyr',
      ],
      python_requires='>=3.7',
      tests_require=tests_require,
      extras_require={'test': tests_require,
                      'cli': cli_requires,
                      'fuzz': fuzz_requires,
                     },
      scripts=[],
     )
