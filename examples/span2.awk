BEGIN { FS="\|" }

/\|/ {
    p=match($2, "\\^+")
    if(p) {
        colstart=RSTART-1
        col=colstart+RLENGTH
        printf("%d-%d\n", colstart, col)
    }
}
