#!/usr/bin/awk -f

/^[^│├└]/ {
    dir_count = 0
    print
    next
}

{
    dir_count++
    if (dir_count <= 5) {
        print
    } else if (dir_count == 6) {
        print "    ... (more files)"
    }
}