#!/usr/bin/env python3

AUXILIARY = [
    'Blank.hs',
    'Facade.hs',
    'Sowpods.hs',
    'Types.hs',
]

VERBOSE=False

MODULE_WHITELIST=frozenset([

    # custom
    'Sowpods', 'Types', 'Blank',

    # base
    'Control.Applicative', 'Control.Arrow', 'Control.Category', 'Control.Concurrent', 'Control.Concurrent.Chan', 'Control.Concurrent.MVar', 'Control.Concurrent.QSem', 'Control.Concurrent.QSemN', 'Control.Exception', 'Control.Exception.Base', 'Control.Monad', 'Control.Monad.Fail', 'Control.Monad.Fix', 'Control.Monad.IO.Class', 'Control.Monad.Instances', 'Control.Monad.ST', 'Control.Monad.ST.Lazy', 'Control.Monad.ST.Lazy.Safe', 'Control.Monad.ST.Lazy.Unsafe', 'Control.Monad.ST.Safe', 'Control.Monad.ST.Strict', 'Control.Monad.ST.Unsafe', 'Control.Monad.Zip', 'Data.Bifunctor', 'Data.Bits', 'Data.Bool', 'Data.Char', 'Data.Coerce', 'Data.Complex', 'Data.Data', 'Data.Dynamic', 'Data.Either', 'Data.Eq', 'Data.Fixed', 'Data.Foldable', 'Data.Function', 'Data.Functor', 'Data.Functor.Classes', 'Data.Functor.Compose', 'Data.Functor.Const', 'Data.Functor.Identity', 'Data.Functor.Product', 'Data.Functor.Sum', 'Data.IORef', 'Data.Int', 'Data.Ix', 'Data.Kind', 'Data.List', 'Data.List.NonEmpty', 'Data.Maybe', 'Data.Monoid', 'Data.Ord', 'Data.Proxy', 'Data.Ratio', 'Data.STRef', 'Data.STRef.Lazy', 'Data.STRef.Strict', 'Data.Semigroup', 'Data.String', 'Data.Traversable', 'Data.Tuple', 'Data.Type.Bool', 'Data.Type.Coercion', 'Data.Type.Equality', 'Data.Typeable', 'Data.Typeable.Internal', 'Data.Unique', 'Data.Version', 'Data.Void', 'Data.Word', 'Debug.Trace', 'Foreign', 'Foreign.C', 'Foreign.C.Error', 'Foreign.C.String', 'Foreign.C.Types', 'Foreign.Concurrent', 'Foreign.ForeignPtr', 'Foreign.ForeignPtr.Safe', 'Foreign.ForeignPtr.Unsafe', 'Foreign.Marshal', 'Foreign.Marshal.Alloc', 'Foreign.Marshal.Array', 'Foreign.Marshal.Error', 'Foreign.Marshal.Pool', 'Foreign.Marshal.Safe', 'Foreign.Marshal.Unsafe', 'Foreign.Marshal.Utils', 'Foreign.Ptr', 'Foreign.Safe', 'Foreign.StablePtr', 'Foreign.Storable', 'Numeric', 'Numeric.Natural', 'Prelude', 'System.CPUTime', 'System.Console.GetOpt', 'System.Environment', 'System.Exit', 'System.IO', 'System.IO.Error', 'System.IO.Unsafe', 'System.Info', 'System.Timeout', 'Text.ParserCombinators.ReadP', 'Text.ParserCombinators.ReadPrec', 'Text.Printf', 'Text.Read', 'Text.Read.Lex', 'Text.Show', 'Text.Show.Functions', 'Unsafe.Coerce',

    # mtl
    'Control.Monad.Cont', 'Control.Monad.Cont.Class', 'Control.Monad.Error', 'Control.Monad.Error.Class', 'Control.Monad.Except', 'Control.Monad.Identity', 'Control.Monad.List', 'Control.Monad.RWS', 'Control.Monad.RWS.Class', 'Control.Monad.RWS.Lazy', 'Control.Monad.RWS.Strict', 'Control.Monad.Reader', 'Control.Monad.Reader.Class', 'Control.Monad.State', 'Control.Monad.State.Class', 'Control.Monad.State.Lazy', 'Control.Monad.State.Strict', 'Control.Monad.Trans', 'Control.Monad.Writer', 'Control.Monad.Writer.Class', 'Control.Monad.Writer.Lazy', 'Control.Monad.Writer.Strict',

    # random
    'System.Random',

    # deepseq
    'Control.DeepSeq',
])

import os, os.path, sys

def error(fmt, *args, **kw):
    print("ERROR: " + fmt.format(*args, **kw))
    sys.exit(1)

def banner():
    print(
"""

Presubmit test, v3.

This script will look at your Scrabble.hs to see whether we can accept it as
is. It will either give you an error, or it will say that you are ready for
submission. We expect that most people will already be ready for submission;
this presubmit script is meant to catch out any things you might have done
that we didn't foresee, that might cause it to be unmarkable.

If it's unclear why you're getting an error, check the Facebook group, ask, or
if you need to show your source code, send an email to Bram on bram@bram.xyz.


This script will also try to guess which exercises you have attempted. The
script WILL NOT check whether your answer to an exercise is correct.

-----

To be able to mark your Scrabble.hs,

- we must see if you didn't use any "unsafe" constructs; most probably you
  didn't,
- we must take out the type definitions from your file, and import them from a
  different file,
- we must check whether you didn't change the types of the exercises.

To check these things, we change it in a number of ways. Don't worry, we won't
change anything in the Scrabble.hs in the current directory; we will make a
copy and change that copy.

Some changes we must make:

- We take out the import System.Console.Readline and import System.IO.Unsafe.
- We add a {-# LANGUAGE Safe #-} declaration on top. The changed file should
  still compile.
- We change "import qualified Bram" to "import qualified Blank as Bram", and
  we change out Blank.hs for a slightly different one.
- We add a dummy definition for readline and addHistory.

Please always use the latest version of this script, which you can obtain
by running 'git pull'.

Let's see if everything works the way it should.

""")

# A "scrab" is a list of lines of Scrabble.hs.

def testcmd(scrab, cmd, noaux=False):
    """
    Test whether the command returns a zero status code, after scrab has
    been written to Scrabble.hs, in a temp directory along with the
    auxiliary files.
    """

    if not noaux: assertcmd('rm -rf tmp-presubmit')
    if not noaux: assertcmd('mkdir tmp-presubmit')
    if not noaux: assertcmd('cp -t tmp-presubmit {}'.format(
        ' '.join('presubmit-res/'+f for f in AUXILIARY)
    ))

    f = open('tmp-presubmit/Scrabble.hs', 'w')
    for line in scrab:
        print(line, file=f)
    f.close()

    assertcmd(cmd)

    pass

def assertcmd(cmd):
    """Run cmd with system, assert that it returned status 0."""
    if VERBOSE:
        print("+ {}".format(cmd))
    ret = os.system(cmd)
    if ret != 0:
        error("Command {!r} in directory {!r} failed, returned status {}",
            cmd,
            os.getcwd(),
            ret)

def readscrab():
    f = open("Scrabble.hs")
    lines = [line.rstrip() for line in f.readlines()]
    f.close()
    return lines

STEP=0
def printstep(msg):
    global STEP
    STEP += 1
    print("Step {}: {}".format(STEP, msg))

def succ(msg, *args, **kw):
    print("  \u2713 " + msg.format(*args, **kw))

def prog(msg, *args, **kw):
    print("  - " + msg.format(*args, **kw) + "...")

def firstmatch(patt, s, anywhere=False):
    for i, line in enumerate(s):
        stripped = stripcomment(line).strip()
        test = patt in stripped if anywhere else patt == stripped
        if test:
            return i
    return None

def stripcomment(line):
    """Strip off everything after the first '--', as well as the '--'."""
    a, _, b = line.partition("--")
    return a

def main():
    if not os.path.exists("Scrabble.hs"):
        error("Scrabble.hs not found. It must exist in the current directory.")

    banner()

    s = list(readscrab()) # MUTABLE list

    step_bram_blank(s)
    step_readline_unsafeio(s)
    step_safe(s)
    step_types(s)
    step_harness(s)
    step_modules(s)

    print()
    print("SUCCESS. It seems that everything will go smoothly when we will test it.")


def step_bram_blank(s):
    printstep("Changing Bram to Blank, and compiling.")

    m = firstmatch("import qualified Bram", s)
    if m == None:
        succ("no such line found")
    else:
        s[m] = "import qualified Blank as Bram"
        succ("changed line {}.", m)

    testcmd(s, "cd tmp-presubmit && ghc Scrabble")
    succ("compiled.")

def step_readline_unsafeio(s):
    printstep("Removing dependency on readline and addHistory, and compiling.")

    m = firstmatch("import System.Console.Readline", s)
    if m == None:
        succ("no such line found")
    else:
        del s[m]
        succ("removed line {}.", m)

        s += [
            "readline :: String -> IO (Maybe String)",
            "readline p = putStrLn p >> (liftM Just getLine)",
            "addHistory l = return ()",
        ]

        succ("added substitute definitions for readline and addHistory")

    testcmd(s, "cd tmp-presubmit && ghc Scrabble")
    succ("compiled.")

    printstep("Removing dependency on System.IO.Unsafe, and compiling.")

    m = firstmatch("import System.IO.Unsafe", s)
    if m == None:
        succ("no such line found")
    else:
        del s[m]
        succ("removed line {}.", m)

    testcmd(s, "cd tmp-presubmit && ghc Scrabble")
    succ("compiled.")

def step_safe(s):
    printstep("Adding {-# LANGUAGE Safe #-}.")

    s[0:0] = ["{-# LANGUAGE Safe #-}"]
    succ("added.")
    testcmd(s, "cd tmp-presubmit && ghc Scrabble")
    succ("compiled.")

def step_types(s):
    printstep("Compile against Types.hs.")

    m = firstmatch("where", s, anywhere=True)
    if m == None:
        error("could not find 'where' in your program")
    s[m+1:m+1] = ["import Types ()"] # dummy import

    succ("added line {}: {!r}.", m, "import Types ()")
    testcmd(s, "cd tmp-presubmit && ghc Scrabble")
    succ("compiled.")

    s[m+1] = "import Types" # not dummy any more
    succ("added line {}: {!r}.", m, "import Types")

    for desc, fragment_lines in get_type_fragments():
        prog("taking out \"{}\"", desc)

        take_out(s, desc, fragment_lines)
        testcmd(s, "true # write file to disk", noaux=True)

    testcmd(s, "cd tmp-presubmit && ghc Scrabble")
    succ("compiled.")

def step_harness(s):
    printstep("Compiling against dummy test harness.")

    testcmd(s, "cd tmp-presubmit && runhaskell Facade.hs 0")

    printstep("Inspecting which exercises you attempted.")

    testcmd(s, "cd tmp-presubmit && runhaskell Facade.hs 1")

    assertcmd("rm -rf tmp-presubmit")
    succ("removed temporary directory 'tmp-presubmit'")

def step_modules(s):
    printstep("Checking which modules are imported.")
    module_lines = [line.strip() for line in s if line.strip().startswith("import")]

    def module_from_line(line):
        if line.startswith("import qualified "):
            line = line[len("import qualified "):]
        elif line.startswith("import "):
            line = line[len("import "):]
        else:
            error("could not parse import line: " + repr(line))

        module, _, _ = line.partition(' ')
        return module

    modules = list(map(module_from_line, module_lines))

    if not (set(modules) <= MODULE_WHITELIST):
        error("You are importing modules which we have not explicitly marked as okay: {}. Please ask on Facebook whether they are okay.", ', '.join(set(modules) - MODULE_WHITELIST))

    succ("using only whitelisted modules")

def get_type_fragments():
    f = open("presubmit-res/Types.hs")
    lines = f.readlines()
    f.close()

    # horribly inefficient

    while lines[0].strip() != '-----':
        del lines[0]

    fragments = [] # list of pairs (desc, fragment_lines)

    while lines != []:
        assert lines[0].strip() == '-----'
        del lines[0]

        desc = lines[0][3:].strip()
        del lines[0]
        i = 0
        while i < len(lines) and lines[i].strip() != '-----':
            lines[i] = stripcomment(lines[i]).rstrip()
            if lines[i].strip() == '':
                del lines[i]
                continue
            i += 1
        fragment_lines = [line.rstrip() for line in lines[:i]]
        lines = lines[i:]
        fragments.append((desc, fragment_lines))

    return fragments

def take_out(s, desc, fragment_lines):
    i = 0 # in s
    real_i = 0 # in s
    taken_out_i = []

    j = 0 # in fragment_lines

    while j < len(fragment_lines):
        if i >= len(s):
            error("could not find fragment as it is in the original Scrabble.hs: {}", desc)
        if stripcomment(s[i]).strip() == fragment_lines[j].strip():
            taken_out_i.append(real_i)
            del s[i]
            j += 1
        else:
            i += 1

        real_i += 1

    succ("taken out lines {}", taken_out_i)





if __name__ == '__main__':
    main()

