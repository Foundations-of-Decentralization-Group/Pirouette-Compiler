#! /bin/sh

gen_proc_list()
{
    procs=""
    i=0
    while [ $i -lt $1 ]; do
        i=$(($i + 1))
        procs="$procs p$i"
    done
    printf "$procs"
}

indent()
{
    if [ $1 -gt 0 ]; then
        i=0
        while [ $i -lt $1 ]; do
            indent="$indent "
            i=$(($i + 1))
        done
    fi
    if [ $1 -lt 0 ]; then
        indent=$(printf "$indent" | cut -c$(($1 * -1 + 1))-)
    fi
    return
}

println()
{
    printf "$indent$@\n"
    return
}

fan_out()
# $1..$n : Process List
# $label : dynamically scoped variable (Like a parameter in Racket)
{
    if [ $# -gt 1 ]; then
        println "$1[$label] ~> $2;"
        local x2=$2
        if [ $# -gt 2 ]; then
            println "$1[$label] ~> $3;"
            local x3=$3
            shift 3
            half=$(($# / 2))
            i=0
            arg_first_half=''
            while [ $i -lt $half ]; do
                arg_first_half="$arg_first_half $1"
                shift 1
                i=$(($i + 1))
            done
            fan_out $x2 $arg_first_half
            fan_out $x3 $@
        fi
    fi
    return
}

fan_in()
# $1..$n : Process List
# $label
{
    if [ $# -gt 1 ]; then
        local x1=$1
        local x2=$2
        if [ $# -gt 2 ]; then
            local x3=$3
            shift 3
            half=$(($# / 2))
            i=0
            arg_first_half=''
            while [ $i -lt $half ]; do
                arg_first_half="$arg_first_half $1"
                shift 1
                i=$(($i + 1))
            done
            fan_in $x2 $arg_first_half
            fan_in $x3 $@
            println "let $x1.res_2 := [$x3] $x3.2 ~> $x1; in"
        fi
        println "let $x1.res_1 := [$x2] $x2.1 ~> $x1; in"
    fi
    return
}

seq_out()
{
    proc=$1
    shift 1
    i=0
    while [ $# -ge 1 ]; do
        println "$proc[$label] ~> $1;"
        shift 1
    done
    return
}

seq_in()
{
    proc=$1
    shift 1
    i=0
    while [ $# -ge 1 ]; do
        i=$(($i + 1))
        println "let $proc.res_$i := [$1] $1.$i ~> $proc; in"
        shift 1
    done
    return
}

none_in()
{
    return
}

gen_prog()
# $1 : number participants
# $2 : scatter code gen function
# $3 : gather code gen function
# $4 : num iters
{
    indent=""
    proc_list=$(gen_proc_list $1)
    println 'foreign gettimeofday : unit -> unit := "@Unix:gettimeofday";'
    println 'foreign print_float : unit -> unit := "@Stdlib:print_float";'
    println 'foreign sub_float : unit -> unit -> unit := "@Stdlib:(-.)";'
    println ''
    println "loop iter :="
    indent 4
    println "if p1.(iter > 0) then"
    indent 4
    label=L
    $2 $proc_list
    $3 $proc_list
    println "loop p1.(iter - 1)"
    indent -4
    println "else"
    indent 4
    label=R
    $2 $proc_list
    $3 $proc_list
    println "p1.();"
    indent -8
    println ""
    println "main :="
    indent 4
    println "let p1.start_time := p1.gettimeofday p1.(); in"
    println "let p1._ := loop p1.$4; in"
    println "let p1.end_time := p1.gettimeofday p1.(); in"
    println "let p1.time_diff := p1.sub_float p1.end_time p1.start_time; in"
    println "p1.print_float p1.time_diff;"
    return
}

gen_prog_variants()
# $1 : num participants
# $2 : num iters
# $3 : output dir
{
    local participants=$1
    local iters=$2
    local out_dir=$3
    gen_prog $participants seq_out fan_in $iters > $out_dir/p${participants}_seq_out_fan_in.pir
    gen_prog $participants fan_out fan_in $iters > $out_dir/p${participants}_fan_out_fan_in.pir
    gen_prog $participants seq_out seq_in $iters > $out_dir/p${participants}_seq_out_seq_in.pir
    gen_prog $participants fan_out seq_in $iters > $out_dir/p${participants}_fan_out_seq_in.pir
    gen_prog $participants seq_out none_in $iters > $out_dir/p${participants}_seq_out_none_in.pir
    gen_prog $participants fan_out none_in $iters > $out_dir/p${participants}_fan_out_none_in.pir
}

out_dir=progs
mkdir -p $out_dir
iters=1000000

gen_prog_variants 2 $iters $out_dir
gen_prog_variants 3 $iters $out_dir
gen_prog_variants 4 $iters $out_dir

gen_prog_variants 6 $iters $out_dir
gen_prog_variants 7 $iters $out_dir
gen_prog_variants 8 $iters $out_dir

gen_prog_variants 14 $iters $out_dir
gen_prog_variants 15 $iters $out_dir
gen_prog_variants 16 $iters $out_dir

gen_prog_variants 30 $iters $out_dir
gen_prog_variants 31 $iters $out_dir
gen_prog_variants 32 $iters $out_dir

gen_prog_variants 62 $iters $out_dir
gen_prog_variants 63 $iters $out_dir
gen_prog_variants 64 $iters $out_dir
