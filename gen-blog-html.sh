#!/usr/bin/env bash

cd blog || exit 1
for slug in *.md; do
    [ -f "$slug" ] || continue
    echo -n "$slug... "
    pandoc "$slug" -o "${slug%.md}.html"
    echo "Converted to ${slug%.md}.html"
done
