{
    print $0
    split($0, f, "--", seps)
    match($f[1], /^.*\(\"(.*)\", *(.+)\).*/, arr)
    gsub(/^[ \t]/, "", arr[2])
    gsub(/^[ \t]/, "", f[2])
    #if (length(arr[1]) > 0){
    if (length(f[2]) > 0) {
        cmd=f[2]
    } else {
        cmd=arr[2]
    }
    #printf ("    ^fg(%s)%6s ^fg(%s)%s\n", keycolor, arr[1], cmdcolor, cmd)
    #}
}
