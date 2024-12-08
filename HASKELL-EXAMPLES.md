# Get Latest Version from Hackage

```zsh
cabal info splitmix | \
    ja --header -R'\n[^:\n]*:' -F'\s*,\s*' '[x ~* 1 /(\d+(\.\d+)*)/]:?{%/Versions available:/}{[y]|>`$}'
```
