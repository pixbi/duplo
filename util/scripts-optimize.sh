#!/usr/bin/env bash

echo "$(cat)" | $DUPLO_UTIL"amd.js" | $DUPLO_NODEJS"uglifyjs" --mangle ----consolidate-primitive-values --compress sequences,properties,dead_code,drop_debugger,unsafe,conditionals,comparisons,evaluate,booleans,loops,unused,hoist_funs,if_return,join_vars,cascade,negate_iife,drop_console | cat
