package fun.gravax.aa.yafl.full

private trait YaflUriStuff

/*
YaflUri represents a Uri used to name some Yafl entity, which is either Data, PureOneArgFunction, or Type.

Given a space of allowed data(types), the spaces of allowed functions and types may be defined inductively (and
preferably constructively?) in terms of that data to complete a type-level universe, eg. Universe0101

Then we may define partial functions from YaflUri to Universe0101.
Names may not be presumed unique.
However, once instantiated in a runtime context, YaflUris and their referents are always immutable.

Note that an implementation like AxLam might explicitly choose some well known Uris for use in a runtime ctx,
to facilitate encoding and computation.
 */
trait YaflUri



