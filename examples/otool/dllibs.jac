{. otool -l $(locate libapple.dylib) | ja run dllibs.jac
:set rs:=/Load command/;
:set fs:=/ *\n */;

.?{`2 ~ /^cmd LC_LOAD_DYLIB$/}{`0 ~* 1 /\n *name ([^\s]+)/}
