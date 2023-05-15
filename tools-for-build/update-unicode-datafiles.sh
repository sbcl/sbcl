#! /bin/bash

UCDIR="$1"
declare -A SKIP FOUND

for file in tools-for-build/*.txt tests/data/*.txt; do
    f="$(basename "$file")"
    for dir in "" auxiliary extracted CollationTest emoji; do
        if [ -z "$dir" ]; then
            t="$UCDIR/$f"
        else
            t="$UCDIR/$dir/$f"
        fi
        if [ -f "$t" ]; then
            echo "Required file $f found: $t"
            FOUND[$file]="$t"
            break
        elif [ "true" = "${SKIP[$f]}" ]; then
            echo "Skipping file $f"
            break
        fi
    done
    if [ -z "${FOUND[$file]}" -a -z "${SKIP[$f]}" ]; then
            echo "Required file not found in $UCDIR: $f"
            exit 1
    fi
done

for file in "${!FOUND[@]}"; do
    cp "${FOUND[$file]}" "$file"
done
