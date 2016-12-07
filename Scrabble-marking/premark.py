#!/usr/bin/env python3

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

Premark test.

""")

# A "scrab" is a list of lines of Scrabble.hs.

def testcmd(scrab, cmd, noaux=False):
    """
    Test whether the command returns a zero status code, after scrab has
    been written to Scrabble.hs, in a temp directory along with the
    auxiliary files.
    """

    f = open('Scrabble.hs', 'w')
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
    step_above1st(s)
    step_types(s)
    step_harness(s)
    step_modules(s)

    print()
    print("SUCCESS. It seems that everything will go smoothly when we will test it.")


def step_bram_blank(s):
    printstep("Changing Bram to Blank.")

    m = firstmatch("import qualified Bram", s)
    if m == None:
        succ("no such line found")
    else:
        s[m] = "import qualified Blank as Bram"
        succ("changed line {}.", m)


def step_readline_unsafeio(s):
    printstep("Removing dependency on readline and addHistory.")

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


    printstep("Removing dependency on System.IO.Unsafe.")

    m = firstmatch("import System.IO.Unsafe", s)
    if m == None:
        succ("no such line found")
    else:
        del s[m]
        succ("removed line {}.", m)


def step_safe(s):
    printstep("Adding {-# LANGUAGE Safe #-}.")

    s[0:0] = ["{-# LANGUAGE Safe #-}"]
    succ("added.")

def step_above1st(s):

    printstep("Making sure new function names are present.")

    if any("LetterStream" in line for line in s):
        succ("already present.")
    else:
        # First, add footer.
        f = open("above1st.hs.footer")
        s += [line.rstrip() for line in f.readlines()]
        f.close()
        succ("added footer.")

        m = firstmatch("where", s, anywhere=True)
        if m == None:
            error("could not find 'where' in your program")
        s[m+1:m+1] = ["import Control.Monad.State"]
        s[m+1:m+1] = ["import Data.Either"]

        succ("added imports.")

def step_types(s):
    printstep("Compile against Types.hs.")

    m = firstmatch("where", s, anywhere=True)
    if m == None:
        error("could not find 'where' in your program")
    s[m+1:m+1] = ["import Types ()"] # dummy import

    succ("added line {}: {!r}.", m, "import Types ()")

    s[m+1] = "import Types" # not dummy any more
    succ("added line {}: {!r}.", m, "import Types")

    for desc, fragment_lines in get_type_fragments():
        prog("taking out \"{}\"", desc)

        take_out(s, desc, fragment_lines)
        testcmd(s, "true # write file to disk", noaux=True)


def step_harness(s):
    printstep("Compiling against dummy test harness.")

    testcmd(s, "runhaskell Facade.hs 0")

    printstep("Inspecting which exercises you attempted.")

    testcmd(s, "runhaskell Facade.hs 1")


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
    f = open("Types.hs")
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

