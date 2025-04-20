#!/bin/bash
pid=$1
echo $1
echo $pid
watch -n.5 grep ctxt /proc/$pid/status
