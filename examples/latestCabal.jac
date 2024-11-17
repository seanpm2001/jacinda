:set header;
:set rs:=/\n[^:\n]*:/;
:set fs:=/\s*,\s*/;

[x ~* 1 /(\d+(\.\d+)*)/]:?{%/Versions available:/}{[y]|>`$}
