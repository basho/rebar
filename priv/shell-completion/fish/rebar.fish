## fish completions for rebar 2.5.0

function __fish_rebar_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'rebar' ]
    return 0
  end
  return 1
end

function __fish_rebar_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

## Rebar Command Output
## ➜  ~  rebar --version
## rebar 2.5.0 R16B02 20140716_213805 git 2.5.0-14-g1e7c742
## ➜  ~  rebar --help
## Usage: rebar [-h] [-c] [-v <verbose>] [-q <quiet>] [-V] [-f]
##              [-D <defines>] [-j <jobs>] [-C <config>] [-p] [-k]
##              [-r <recursive>] [var=value,...] <command,...>
##
##   -h, --help        Show the program options
##   -c, --commands    Show available commands
##   -v, --verbose     Verbosity level (-v, -vv)
##   -q, --quiet       Quiet, only print error messages
##   -V, --version     Show version information
##   -f, --force       Force
##   -D                Define compiler macro
##   -j, --jobs        Number of concurrent workers a command may use.
##                     Default: 3
##   -C, --config      Rebar config file to use
##   -p, --profile     Profile this run of rebar
##   -k, --keep-going  Keep running after a command fails
##   -r, --recursive   Apply commands to subdirs and dependencies
##   var=value         rebar global variables (e.g. force=1)
##   command           Command to run (e.g. compile)
##
## To see a list of built-in commands, execute rebar -c.
##
## Type 'rebar help <CMD1> <CMD2>' for help on specific commands.

# general options
complete -f -c rebar -n 'not __fish_rebar_needs_command' -l help -d 'Display the manual of a rebar command'

## Flags
complete -c rebar -s h -l help        -d "Show the program options"
complete -c rebar -s c -l commands    -d "Show available commands"
complete -c rebar -s v -l verbose     -d "Verbosity level (-v, -vv, -vvv, --verbose 3). Default: 0"
complete -c rebar -s vv               -d "Verbosity level 2"
complete -c rebar -s vvv              -d "Verbosity level 2"
complete -c rebar -s q -l quiet       -d "Quiet, only print error messages"
complete -c rebar -s V -l version     -d "Show version information"
complete -c rebar -s f -l force       -d "Force"
complete -c rebar -s D                -d "Define compiler macro"
complete -c rebar -s j -l jobs        -d "Number of concurrent workers a command may use. Default: 3"
complete -c rebar -s C -l config      -d "Rebar config file to use"
complete -c rebar -s p -l profile     -d "Profile this run of rebar"
complete -c rebar -s k -l keep-going  -d "Keep running after a command fails"
complete -c rebar -s r -l recursive   -d "Apply commands to subdirs and dependencies"


## Not going to cover abbreviations, since this is for completions anyway :D
## rebar allows you to abbreviate the command to run:
## $ rebar co           # same as rebar compile
## $ rebar eu           # same as rebar eunit
## $ rebar g-d          # same as rebar get-deps
## $ rebar x eu         # same as rebar xref eunit
## $ rebar l-d          # same as rebar list-deps
## $ rebar l-d l-t      # same as rebar list-deps list-templates
## $ rebar list-d l-te  # same as rebar list-deps list-templates
##
## Core command line options:
##   apps=app1,app2 (specify apps to process)
##   skip_apps=app1,app2 (specify apps to skip)

## COMMANDS!
## ➜  ~  rebar -c
## clean                                    Clean
complete -f -c rebar -n '__fish_rebar_needs_command' -a clean -d 'Clean'

## compile                                  Compile sources
complete -f -c rebar -n '__fish_rebar_needs_command' -a compile -d 'Compile sources'

## escriptize                               Generate escript archive
complete -f -c rebar -n '__fish_rebar_needs_command' -a escriptize -d 'Generate escript archive'

## create      template= [var=foo,...]      Create skel based on template and vars
complete -f -c rebar -n '__fish_rebar_needs_command' -a create -d 'Create skel based on template and vars'
complete -f -c rebar -n '__fish_rebar_using_command create' -a 'template=' -d 'Template name'

## create-app  [appid=myapp]                Create simple app skel
complete -f -c rebar -n '__fish_rebar_needs_command' -a create-app -d 'Create simple app skel'
complete -f -c rebar -n '__fish_rebar_using_command create-app' -a 'appid=' -d 'Application name'

## create-lib  [libid=mylib]                Create simple lib skel
complete -f -c rebar -n '__fish_rebar_needs_command' -a create-lib -d 'Create simple lib skel'
complete -f -c rebar -n '__fish_rebar_using_command create-lib' -a 'libid=' -d 'Library name'

## create-node [nodeid=mynode]              Create simple node skel
complete -f -c rebar -n '__fish_rebar_needs_command' -a create-node -d 'Create simple node skel'
complete -f -c rebar -n '__fish_rebar_using_command create-node' -a 'nodeid=' -d 'Node name'

## list-templates                           List available templates
complete -f -c rebar -n '__fish_rebar_needs_command' -a list-templates -d 'List available templates'

## doc                                      Generate Erlang program documentation
complete -f -c rebar -n '__fish_rebar_needs_command' -a doc -d 'Generate Erlang program documentation'

## prepare-deps                             Run 'rebar -r get-deps compile'
complete -f -c rebar -n '__fish_rebar_needs_command' -a prepare-deps -d 'Prepare Dependencies'

## refresh-deps                             Run 'rebar -r update-deps compile'
complete -f -c rebar -n '__fish_rebar_needs_command' -a refresh-deps -d 'Refresh Dependencies'

## check-deps                               Display to be fetched dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a check-deps -d 'Display to be fetched dependencies'

## get-deps                                 Fetch dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a get-deps -d 'Fetch dependencies'

## update-deps                              Update fetched dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a update-deps -d 'Update fetched dependencies'

## delete-deps                              Delete fetched dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a delete-deps -d 'Delete fetched dependencies'

## list-deps                                List dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a list-deps -d 'List Dependencies'

## generate    [dump_spec=0/1]              Build release with reltool
complete -f -c rebar -n '__fish_rebar_needs_command' -a generate -d 'Build release with reltool'
complete -f -c rebar -n '__fish_rebar_using_command generate' -a 'dump_spec=0 dump_spec=1'

## overlay                                  Run reltool overlays only
complete -f -c rebar -n '__fish_rebar_needs_command' -a overlay -d 'Run reltool overlays only'

## generate-upgrade  previous_release=path  Build an upgrade package
complete -f -c rebar -n '__fish_rebar_needs_command' -a generate-upgrade -d 'Build an upgrade package'
complete -f -c rebar -n '__fish_rebar_using_command generate-upgrade' -a 'previous_release='

## generate-appups   previous_release=path  Generate appup files
complete -f -c rebar -n '__fish_rebar_needs_command' -a generate-appups -d 'Generate appup files'
complete -f -c rebar -n '__fish_rebar_using_command generate-appups' -a 'previous_release='

## eunit       [suite[s]=foo]               Run EUnit tests in foo.erl and
##                                          test/foo_tests.erl
##             [suite[s]=foo] [test[s]=bar] Run specific EUnit tests [first test
##                                          name starting with 'bar' in foo.erl
##                                          and test/foo_tests.erl]
##             [test[s]=bar]                For every existing suite, run the first
##                                          test whose name starts with bar and, if
##                                          no such test exists, run the test whose
##                                          name starts with bar in the suite's
##                                          _tests module.
##             [random_suite_order=true]    Run tests in a random order, either
##             [random_suite_order=Seed]    with a random seed for the PRNG, or a
##                                          specific one.
complete -f -c rebar -n '__fish_rebar_needs_command' -a eunit -d 'Run EUnit tests'
complete -f -c rebar -n '__fish_rebar_using_command eunit' -a 'suites='
complete -f -c rebar -n '__fish_rebar_using_command eunit' -a 'tests='
complete -f -c rebar -n '__fish_rebar_using_command eunit' -a 'random_suite_order='

## ct          [suite[s]=] [case=]          Run common_test suites
complete -f -c rebar -n '__fish_rebar_needs_command' -a ct -d 'Run common_test suites'
complete -f -c rebar -n '__fish_rebar_using_command ct' -a 'suites='
complete -f -c rebar -n '__fish_rebar_using_command ct' -a 'case='

## qc                                       Test QuickCheck properties
complete -f -c rebar -n '__fish_rebar_needs_command' -a qc -d 'Test QuickCheck properties'

## xref                                     Run cross reference analysis
complete -f -c rebar -n '__fish_rebar_needs_command' -a xref -d 'Run cross reference analysis'

## shell                                    Start a shell similar to
##                                          'erl -pa ebin -pa deps/*/ebin'
complete -f -c rebar -n '__fish_rebar_needs_command' -a shell -d 'Start a shell'

## help                                     Show the program options
complete -f -c rebar -n '__fish_rebar_needs_command' -a help -d 'Show the program options'

## version                                  Show version information
complete -f -c rebar -n '__fish_rebar_needs_command' -a version -d 'Show version information'
