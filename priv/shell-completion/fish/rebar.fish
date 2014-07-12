
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

# general options
complete -f -c rebar -n 'not __fish_rebar_needs_command' -l help -d 'Display the manual of a rebar command'


#### clean
complete -f -c rebar -n '__fish_rebar_needs_command' -a clean -d 'Clean'

#### compile
complete -f -c rebar -n '__fish_rebar_needs_command' -a compile -d 'Compile'

#### escriptize
complete -f -c rebar -n '__fish_rebar_needs_command' -a escriptize -d 'Escriptize'

#### create
complete -f -c rebar -n '__fish_rebar_needs_command' -a create -d 'Create'
complete -f -c rebar -n '__fish_rebar_using_command create' -a 'template=' -d 'Template name'

#### create-app  [appid=myapp]            Create simple app skel
complete -f -c rebar -n '__fish_rebar_needs_command' -a create-app -d 'Create simple app skel'
complete -f -c rebar -n '__fish_rebar_using_command create-app' -a 'appid=' -d 'Application name'

#### create-node [nodeid=mynode]          Create simple node skel
complete -f -c rebar -n '__fish_rebar_needs_command' -a create-node -d 'Create simple node skel'
complete -f -c rebar -n '__fish_rebar_using_command create-node' -a 'nodeid=' -d 'Node name'

#### list-templates                       List available templates
complete -f -c rebar -n '__fish_rebar_needs_command' -a list-templates -d 'List available templates'

#### doc                                  Generate Erlang program documentation
complete -f -c rebar -n '__fish_rebar_needs_command' -a doc -d 'Generate Erlang program documentation'

#### check-deps                           Display to be fetched dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a check-deps -d 'Display to be fetched dependencies'

#### get-deps                             Fetch dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a get-deps -d 'Fetch dependencies'

#### update-deps                          Update fetched dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a update-deps -d 'Update fetched dependencies'

#### delete-deps                          Delete fetched dependencies
complete -f -c rebar -n '__fish_rebar_needs_command' -a delete-deps -d 'Delete fetched dependencies'

#### list-deps
complete -f -c rebar -n '__fish_rebar_needs_command' -a list-deps -d 'List Dependencies'

#### generate    [dump_spec=0/1]          Build release with reltool overlay
complete -f -c rebar -n '__fish_rebar_needs_command' -a generate -d 'Build release with reltool overlay'
complete -f -c rebar -n '__fish_rebar_using_command generate' -a 'dump_spec=0 dump_spec=1'


#### generate-upgrade  previous_release=path  Build an upgrade package
complete -f -c rebar -n '__fish_rebar_needs_command' -a generate-upgrade -d 'Build an upgrade package'
complete -f -c rebar -n '__fish_rebar_using_command generate-upgrade' -a 'previous_release='

#### generate-appups   previous_release=path  Generate appup files
complete -f -c rebar -n '__fish_rebar_needs_command' -a generate-appups -d 'Generate appup files'
complete -f -c rebar -n '__fish_rebar_using_command generate-appups' -a 'previous_release='

#### eunit       [suites=foo]             Run eunit tests in foo.erl and
####                                      test/foo_tests.erl
####             [suites=foo] [tests=bar] Run specific eunit tests [first test name
####                                      starting with 'bar' in foo.erl and
####                                      test/foo_tests.erl]
####             [tests=bar]              For every existing suite, run the first
####                                      test whose name starts with bar and, if
####                                      no such test exists, run the test whose
####                                      name starts with bar in the suite's
####                                      _tests module
complete -f -c rebar -n '__fish_rebar_needs_command' -a eunit -d 'EUnit'
complete -f -c rebar -n '__fish_rebar_using_command eunit' -a 'suites='
complete -f -c rebar -n '__fish_rebar_using_command eunit' -a 'tests='

#### ct          [suites=] [case=]        Run common_test suites
complete -f -c rebar -n '__fish_rebar_needs_command' -a ct -d 'Common Test'
complete -f -c rebar -n '__fish_rebar_using_command ct' -a 'suites='
complete -f -c rebar -n '__fish_rebar_using_command ct' -a 'case='


#### qc
complete -f -c rebar -n '__fish_rebar_needs_command' -a qc -d 'QuickCheck'

#### xref
complete -f -c rebar -n '__fish_rebar_needs_command' -a xref -d 'xref'

#### help
complete -f -c rebar -n '__fish_rebar_needs_command' -a help -d 'Help!'

#### version
complete -f -c rebar -n '__fish_rebar_needs_command' -a version -d 'Version'

## Flags
complete -c rebar -s h -l help        -d "Show the program options"
complete -c rebar -s c -l commands    -d "Show available commands"
complete -c rebar -s v -l verbose     -d "Verbosity level (-v, -vv, -vvv, --verbose 3). Default: 0"
complete -c rebar -s V -l version     -d "Show version information"
complete -c rebar -s f -l force       -d "Force"
complete -c rebar -s D                -d "Define compiler macro"
complete -c rebar -s j -l jobs        -d "Number of concurrent workers a command may use. Default: 3"
complete -c rebar -s C -l config      -d "Rebar config file to use"
complete -c rebar -s p -l profile     -d "Profile this run of rebar"
complete -c rebar -s k -l keep-going  -d "Keep running after a command fails"
