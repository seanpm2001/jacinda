:set rs:=/\n\n/;
:set fs:=/\n/;

fn present(rec) :=
  sprintf '%s %s\n%s\n' (rec->level . rec->loc . rec->msg);

fn annoying(err) :=
    err->code !~ /GHC-06201|unrecognised-warning-flags/
  & err->msg !~ /not used: record field/;

fn ignorance(err) :=
    err->code !~ /incomplete\-(uni\-)?patterns|GHC-61505/;

fn flib(x) :=
  x->msg !~ /Illegal foreign declaration/;

present¨
ignorance#.
flib#.
[x->level ~ /error/ || annoying(x)]#.
  {| let
      val file := option '' [x] (`1 ~* 1 /^([\p{Letter}\/\.\-\pNd]+)/)
      val msg := [x+'\n'+y]|>([x~/^\s+(\p{Letter}|•|_|\-|\.{3}|\(|\[|:)/]#.`$)
      val lc := option '' [':'+x] (`1 ~* 1 /(\d+:\d+):/)
      val span := option '' [sprintf '-%i' (#x)] (`* ~* 1 /(\^+)/)
      val code := drop# 1 (split `1 /(\[|\n|\])/)
    in
      #{ level .= (split `1 /( :|: )/).2
       ; code .= (+)|⍬ code
       ; loc .= sprintf '%s%s%s' (file.lc.span)
       ; file .= file; msg .= msg
       }
    end}
