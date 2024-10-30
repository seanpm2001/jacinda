fn prettyMem(x) :=
  ?x>=1073741824.0
  ;sprintf'%f.2 GB' (x%1073741824.0)
  ;?x>=1048576.0
   ;sprintf'%f.2 MB' (x%1048576.0)
   ;?x>=1024.0
    ;sprintf'%f.2 kB' (x%1024.0)
    ;sprintf'%f.0 b' x;

{. du reports memory in bytes by default
fn duMem(x) := prettyMem(x*1024.0);
