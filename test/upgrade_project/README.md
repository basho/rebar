#### Building version 0.1
    rebar compile
    cd rel
    rebar generate
    mv dummy dummy_0.1
    cd ..
    rebar clean
    # start the release:
    cd rel/dummy_0.1
    bin/dummy console

    erl> dummy_server:get_state().
    erl> dummy_server:set_state(123).
    erl> dummy_server:get_state().

#### Building version 0.2

    # Now, in another terminal we prepare an upgrade..

    # change release version numbers from 0.1 to 0.2 in
    $EDITOR apps/dummy/src/dummy.app.src
    $EDITOR rel/reltool.config

    rebar compile
    cd rel
    rebar generate
    # previous_release path is relative to your rel directory
    rebar generate-appups previous_release=dummy_0.1
    rebar generate-upgrade previous_release=dummy_0.1
    tar -zvtf dummy_0.2.tar.gz
    mv dummy dummy_0.2


#### Deploying with release_handler
    mv dummy_0.2.tar.gz dummy_0.1/releases/

    # Now use release_handler in the running erlang console for the deploy:

    erl> release_handler:unpack_release("dummy_0.2").
    erl> release_handler:install_release("0.2").
    erl> release_handler:make_permanent("0.2").

    erl> release_handler:which_releases().
    erl> dummy_server:get_state().

#### Building version 0.3
    rm -r rel/dummy

    # Now repeat steps in 'Building version 0.2' and 'Deploying with release_handler'
    # while replacing '0.2' by '0.3' and '0.1' by '0.2'.
