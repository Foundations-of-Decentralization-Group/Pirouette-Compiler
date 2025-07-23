#! /bin/sh

compile_cmd="dune exec -- pirc"

while getopts "b:p:o:c:hrg" opt
do
    case $opt in
        "r")
            run="true"
            ;;
        "g")
            gflag=-g
            ;;
        "b")
            backend=${OPTARG}
            ;;
        "p")
            processes=`printf ${OPTARG} | awk '{print $0}' RS=',' ORS=' '`
            num_processes=`printf ${OPTARG} | awk 'END {print NR}' RS=','`
            num_procs_flag="-n ${num_processes}"
            ;;
        "o")
            output_dir=${OPTARG}
            ;;
        "c")
            compile_cmd=${OPTARG}
            ;;
        "h")
            echo "Usage: $0 -b <backend> [-p|-r|-h] <file>"
            echo ""
            echo "Flags:"
            echo "  -b <backend>       : select backend ('domain', 'http', or 'mpi')"
            echo "  -p <process list>  : The list of processes that are involved, written with commas between"
            echo "                        each process name. For example: '-p Foo,Bar,Baz'".
            echo "                       If the 'http' backend is selected then this option is required."
            echo "                       If the 'mpi' backend is selected, this option is strongly recommended"
            echo "                        (but not strictly required) since the number of processes listed"
            echo "                        determines how many processes are spawned by the mpiexec command."
            echo "  -r                 : run the projected program(s)"
            echo "  -g                 : compile projection(s) with the '-g' debug flag"
            echo "  -o <dir>           : directory to put the resulting '.exe' files (TODO: update to effect .ml files too)"
            echo "  -c <cmd>           : what command to use to call the Pirouette compiler (default: 'dune exec -- pirc')"
            echo "  -h                 : display this help message"
            exit 0
            ;;
        "?")
            exit 1
            ;;
    esac
done

dune --version >> /dev/null 2>> /dev/null
if [ $? -ne 0 ]; then
    echo "ERROR, 'dune --version' does not appear to work correctly!"
    echo "Hint: have you run 'eval $(opam env)' in your shell yet to initialize Opam?"
    exit 1
fi

# https://stackoverflow.com/questions/3601515/how-to-check-if-a-variable-is-set-in-bash
if [ -z ${backend+x} ]; then
    echo "ERROR, backend not specified!"
    exit 1
fi

if [ "$backend" = "http" ] && [ -z ${processes+x} ]; then
    echo "ERROR, process list not specified, which is required with the 'http' backend! (Hint: use -p)"
    exit 1
fi

shift $((OPTIND-1))

if [ $# -ne 1 ]; then
    echo "ERROR, exactly one file must be specified!"
    exit 1
fi

file=$1
dir=`dirname ${file}`
if [ -z ${output_dir+x} ]; then
    output_dir=$dir
fi
base=`basename ${file} .pir`

case $backend in
    "domain")
        $compile_cmd -msg-backend=domain $file &&
            ocamlfind ocamlopt $gflag -o $output_dir/$base.domain.exe -linkpkg -package domainslib $dir/$base.domain.ml
        if [ "$run" = "true" ]; then
            ./$output_dir/$base.domain.exe
        fi
        ;;
    "mpi")
        $compile_cmd -msg-backend=mpi $file &&
            ocamlfind ocamlopt $gflag -o $output_dir/$base.mpi.exe -linkpkg -package mpi $dir/$base.mpi.ml
        if [ "$run" = "true" ]; then
            mpiexec --oversubscribe ${num_procs_flag} ./$output_dir/$base.mpi.exe
        fi
        ;;
    "http")
        OCAMLPATH=./_build/install/default/lib/ ocamlfind query http_pirc >> /dev/null 2>> /dev/null
        if [ $? -ne 0 ]; then
            echo "ERROR, cannot locate the 'http_pirc' library."
            echo "This library is used to compile programs projected for the http backend."
            echo "Hint: run 'dune build @install'"
            exit 1
        fi
        $compile_cmd -msg-backend=http $file &&
            for process in $processes; do
                OCAMLPATH=./_build/install/default/lib/ ocamlfind ocamlopt $gflag -o $output_dir/${base}_$process.exe -linkpkg -package http_pirc $dir/${base}_$process.ml
            done &&
            if [ "$run" = "true" ]; then
                for process in $processes; do
                    $output_dir/${base}_$process.exe &
                done
                wait
            fi
        ;;
    *)
        echo "ERROR, invalid backend: ${backend}"
        exit 1
esac
