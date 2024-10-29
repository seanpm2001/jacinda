{. otool -l $(locate libapple.dylib) | ja run rpath.jac
:set rs:=/Load command/;
:set fs:=/ *\n */;

.?{`2 ~ /^cmd LC_RPATH$/}{`0 ~* 1 /\n *path ([^\s]+)/}
