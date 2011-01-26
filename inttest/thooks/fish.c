#include "erl_nif.h"

static ErlNifResourceType* fish_RESOURCE;

typedef struct
{
} fish_handle;

// Prototypes
ERL_NIF_TERM fish_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM fish_foo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, fish_new},
    {"foo", 1, fish_foo}
};

ERL_NIF_TERM fish_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    fish_handle* handle = enif_alloc_resource(env,
                                                    fish_RESOURCE,
                                                    sizeof(fish_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(env, handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


ERL_NIF_TERM fish_foo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void fish_resource_cleanup(ErlNifEnv* env, void* arg)
{
    // Delete any dynamically allocated memory stored in fish_handle
    // fish_handle* handle = (fish_handle*)arg;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    fish_RESOURCE = enif_open_resource_type(env, "fish_resource",
                                                  &fish_resource_cleanup,
                                                  ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                  0);
    return 0;
}

ERL_NIF_INIT(fish, nif_funcs, &on_load, NULL, NULL, NULL);
