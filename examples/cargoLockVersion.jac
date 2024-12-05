:set rs := /\[\[package\]\]/;
:set fs :=/\n+/;

fn any(p,xs) :=
  (||)|>p"xs;

{any (~/name = "libc"/) `$}{(~/version = "/) #. `$}
