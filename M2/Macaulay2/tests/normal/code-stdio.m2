inputFile = temporaryFileName()
outputFile = temporaryFileName()

inputFile << ///1
code a
f = (x -> (
    x + 1
    ))
code f
/// << close

srcdir = replace("Macaulay2/tests/normal/code-stdio\\.m2$", "", currentFileName)
cmd = concatenate(
    format commandLine#0,
    " --srcdir ", format srcdir,
    " --no-readline --silent --no-debug -q",
    " < ", format inputFile,
    " > ", format outputFile,
    " 2>&1")

assert(run cmd == 0)
output = get outputFile
assert not match("error:", output)
assert match("stdio:2:5-2:6: --source code:", output)
assert match("stdio:3:5-5:5: --source code:", output)

cmd = concatenate(
    format commandLine#0,
    " --srcdir ", format srcdir,
    " --webapp --no-readline --silent --no-debug -q",
    " < ", format inputFile,
    " > ", format outputFile,
    " 2>&1")

assert(run cmd == 0)
output = get outputFile
assert not match("line number .* not found in file stdio", output)
assert match("stdio#L2:C5-L2:C6_L2:C5", output)
assert match("stdio#L3:C5-L5:C5_L3:C5", output)

removeFile inputFile
removeFile outputFile
