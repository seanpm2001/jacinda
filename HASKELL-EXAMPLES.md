# Get Latest Version from Hackage

```
cabal info dom-lt | ja -R ':' -F'(\s|,)+' 'last#¨([#*x>0] #. {|(~/^\d+(\.\d+)*$/)#.`$})'
```
