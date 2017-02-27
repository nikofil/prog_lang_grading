#!/bin/sh
cat OursImps.hs $1 Ours.hs > runnable.hs
echo gradMain | ghci runnable.hs
