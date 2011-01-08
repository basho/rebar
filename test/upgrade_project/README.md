#### Testing Upgrade Package Building

    ./rebar compile
    ./rebar generate
    mv rel/dummy rel/dummy_0.1
    ./rebar clean

    # change release version numbers from 0.1 to 0.2 in
    # apps/dummy/src/dummy.app.src
    # rel/reltool.config

    ./rebar compile
    ./rebar generate
    ./rebar upgrade oldreleasepath=dummy_0.1
    tar zxvf rel/dummy_0.2.tar.gz releases/0.2/relupx releases/0.2/relup
    cat releases/0.2/relup
