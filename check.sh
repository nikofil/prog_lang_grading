#!/bin/bash
echo ${@:2}
cpp -w -P "${@:2}" Ours.hs | cat OursImps.hs $1 - > runnable.hs
echo gradMain | ghci runnable.hs
