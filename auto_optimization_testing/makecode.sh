#! /bin/sh

script_name=$0

echo $@
main_args=$@
main()
# $1 : Directory to place programs in (required)
# $2 : Number of iterations (required)
# $3..$n : List of numbers of participants to generate programs for (at least one required)
{
    echo $@
    if [ $# -lt 3 ]; then
        printf "$script_name: Error, too few arguments!\n"
        exit
    fi
    out_dir=$1
    mkdir -p $out_dir
    iters=$2
    shift 2
    participants=$@

    for n in $participants; do
        gen_prog_variants $n $iters $out_dir
    done
}

gen_proc_list()
# $1 : Number of processes
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
# $1 : Amount to change indent by
{
    local indent_by=$1

    local i=0
    while [ $i -lt $indent_by ]; do
        indent="$indent "
        i=$(($i + 1))
    done
    if [ $indent_by -lt 0 ]; then
        indent=$(printf "$indent" | cut -c$(($indent_by * -1 + 1))-)
    fi
    return
}

println()
# $1 : String to print
{
    printf "$indent$@\n"
    return
}

first()
# $1..$n : List of elements
{
    printf $1
    return
}

first_half()
# $1..$n : List of elements
{
    local num=$#
    local half=$(($num - ($num / 2)))
    local i=0
    local proc_first_half=''
    while [ $i -lt $half ]; do
        proc_first_half="$proc_first_half $1"
        shift 1
        i=$(($i + 1))
    done
    printf "$proc_first_half"
    return
}

second_half()
# $1..$n : List of elements
{
    local num=$#
    local half=$(($num - ($num / 2)))
    shift $half
    printf "$*"
    return
}

fan_out_helper()
# $1 : Label
# $2 : Parent Process
# $3 : Current Process
# $4..$n : Rest of Processes
{
    local label=$1
    local parent_proc=$2
    local cur_proc=$3
    shift 3
    local num_children=$#

    println "$parent_proc[$label] ~> $cur_proc;"

    if [ $num_children -eq 0 ]; then
        : # no-op, do nothing
    else
        fan_out_helper $label $cur_proc $(first_half $@)
        if [ $num_children -ge 2 ]; then
            fan_out_helper $label $cur_proc $(second_half $@)
        fi
    fi
}

fan_out()
# $1 : Label
# $2 : Current Process
# $3..$n : Rest of Processes
{
    local label=$1
    local cur_proc=$2
    shift 2
    
    fan_out_helper $label $cur_proc $(first_half $@)
    fan_out_helper $label $cur_proc $(second_half $@)
    return
}

fan_in_helper()
# $1 : Parent Process
# $2 : Current Process
# $3..$n : Rest of Processes
{
    local head_proc=$1
    local cur_proc=$2
    shift 2
    local num_children=$#

    if [ $num_children -eq 0 ]; then
        println "let $head_proc.res_$cur_proc := [$cur_proc] $cur_proc.1 ~> $head_proc; in"
    else
        if [ $num_children -ge 1 ]; then
            # Non-Leaf Nodes
            local proc_first_half=$(first_half $@)
            local proc_second_half=$(second_half $@)
            fan_in_helper $cur_proc $proc_first_half
            if [ $num_children -ge 2 ]; then
                fan_in_helper $cur_proc $proc_second_half
                println "let $cur_proc.sum := $cur_proc.(1 + res_`first $proc_first_half` + res_`first $proc_second_half`); in"
                println "let $head_proc.res_$cur_proc := [$cur_proc] $cur_proc.sum ~> $head_proc; in"
            else
                println "let $cur_proc.sum := $cur_proc.(1 + res_`first $proc_first_half`); in"
                println "let $head_proc.res_$cur_proc := [$cur_proc] $cur_proc.sum ~> $head_proc; in"
            fi
        fi
    fi
    return
}

fan_in()
# $1..$n : Processes
{
    local cur_proc=$1
    shift 1
    
    fan_in_helper $cur_proc $(first_half $@)
    fan_in_helper $cur_proc $(second_half $@)
    return
}

fan_in_collatz_helper()
# $1 : Parent Process
# $2 : Current Process
# $3..$n : Rest of Processes
{
    local head_proc=$1
    local cur_proc=$2
    shift 2
    local num_children=$#

    if [ $num_children -eq 0 ]; then
        println "let $cur_proc.collatz_res := $cur_proc.collatz $cur_proc.931386509544713451; in"
        println "let $head_proc.res_$cur_proc := [$cur_proc] $cur_proc.collatz_res ~> $head_proc; in"
    else
        if [ $num_children -ge 1 ]; then
            # Non-Leaf Nodes
            local proc_first_half=$(first_half $@)
            local proc_second_half=$(second_half $@)
            fan_in_collatz_helper $cur_proc $proc_first_half
            if [ $num_children -ge 2 ]; then
                fan_in_collatz_helper $cur_proc $proc_second_half
                println "let $cur_proc.collatz_res := $cur_proc.collatz $cur_proc.931386509544713451; in"
                println "let $cur_proc.sum := $cur_proc.(collatz_res + res_`first $proc_first_half` + res_`first $proc_second_half`); in"
                println "let $head_proc.res_$cur_proc := [$cur_proc] $cur_proc.sum ~> $head_proc; in"
            else
                println "let $cur_proc.collatz_res := $cur_proc.collatz $cur_proc.931386509544713451; in"
                println "let $cur_proc.sum := $cur_proc.(collatz_res + res_`first $proc_first_half`); in"
                println "let $head_proc.res_$cur_proc := [$cur_proc] $cur_proc.sum ~> $head_proc; in"
            fi
        fi
    fi
    return
}

fan_in_collatz()
# $1..$n : Processes
{
    local cur_proc=$1
    shift 1

    fan_in_collatz_helper $cur_proc $(first_half $@)
    fan_in_collatz_helper $cur_proc $(second_half $@)
    return
}

seq_out()
# $1 : Label
# $2 : Parent Process
# $3..$n : Processes
{
    local label=$1
    local proc=$2
    shift 2
    i=0
    while [ $# -ge 1 ]; do
        println "$proc[$label] ~> $1;"
        shift 1
    done
    return
}

seq_in()
# $1..$n : Processes
{
    local proc=$1
    shift 1
    println "let $proc.sum_0 := $proc.0; in"
    i=0
    while [ $# -ge 1 ]; do
        i_prev=$i
        i=$(($i + 1))
        println "let $proc.temp := [$1] $1.1 ~> $proc; in"
        println "let $proc.sum_$i := $proc.(sum_$i_prev + temp); in"
        shift 1
    done
    return
}

seq_in_collatz()
# $1..$n : Processes
{
    proc=$1
    shift 1
    println "let $proc.sum_0 := $proc.0; in"
    i=0
    while [ $# -ge 1 ]; do
        i_prev=$i
        i=$(($i + 1))
        println "let $1.collatz_res := $1.collatz $1.931386509544713451; in"
        println "let $proc.temp := [$1] $1.collatz_res ~> $proc; in"
        println "let $proc.sum_$i := $proc.(sum_$i_prev + temp); in"
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
# $3 : scatter code gen function
# $4 : gather code gen function
# $5 : num iters
# $6 : Test name / Program type name
{
    indent=""
    proc_list=$(gen_proc_list $1)
    println 'foreign printf : unit -> unit := "@Printf:printf";'
    println 'foreign gettimeofday : unit -> unit := "@Unix:gettimeofday";'
    println 'foreign print_float : unit -> unit := "@Stdlib:print_float";'
    println 'foreign sub_float : unit -> unit -> unit := "@Stdlib:(-.)";'
    println 'foreign collatz : unit -> unit := "@Collatz:collatz";'
    println ''
    println "loop iter :="
    indent 4
    println "if p1.(iter > 0) then"
    indent 4
    $2 L $proc_list
    $3 $proc_list
    println "loop p1.(iter - 1)"
    indent -4
    println "else"
    indent 4
    $2 R $proc_list
    $3 $proc_list
    println "p1.();"
    indent -8
    println ""
    println "main :="
    indent 4
    println "let _ := p1.printf p1.\"$5\"; in"
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

    go=seq_out;gi=fan_in; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=fan_out;gi=fan_in; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=seq_out;gi=seq_in; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=fan_out;gi=seq_in; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir

    go=seq_out;gi=fan_in_collatz; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=fan_out;gi=fan_in_collatz; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=seq_out;gi=seq_in_collatz; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=fan_out;gi=seq_in_collatz; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir

    go=seq_out;gi=none_in; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
    go=fan_out;gi=none_in; gen_prog $participants $go $gi $iters "p${participants}_$gi,$go," > $out_dir/p${participants}_${go}_${gi}.pir
}

main $main_args
