#!/bin/bash

cat ~/notes.txt | sed 's/^/					\${color #ddddff}×\$color /g'
