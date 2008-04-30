#!/bin/bash
#
# Do something like
#
#	find . -type f -name '*.erl' -exec ../scripts/auto-edoc.sh {} \;
#
# from trunk/src/
#

file="$1"

if [ ! -f $file ]; then
    echo "Syntax: $0 file"
    exit 1
fi

dir=$(dirname $file)
file=$(basename $file)

if [ "x$dir" != "x" ]; then
    pushd "$dir" > /dev/null 2>&1
fi

if [ -x ../../scripts/format-for-edoc.pl ]; then
    ../../scripts/format-for-edoc.pl "$file" || exit 1
else
    if [ -x ./scripts/format-for-edoc.pl ]; then
	./scripts/format-for-edoc.pl "$file" || exit 1
    else
	../scripts/format-for-edoc.pl "$file" || exit 1
    fi
fi

if [ ! -f "new_${file}" ]; then
    exit 1
fi

mv -f "new_${file}" "$file"
#mod=$(echo $file | cut -d . -f 1)
#perl -p -i -e "s/^\-module.*\$/-module\(new_${mod}\)./" "new_${file}"

#echo "finished with $file"

if [ "x$dir" != "x" ]; then
    popd > /dev/null 2>&1
fi


