#! /bin/sh

while getopts "b:p:hr" opt
do
    case $opt in
        "r")
            run="true"
            ;;
        "b")
            backend=${OPTARG}
            ;;
        "p")
            OLDIFS=$IFS
            processes=`IFS=','; echo ${OPTARG}`
            IFS=$OLDIFS
            ;;
        "h")
            echo "Usage: $0 -b <backend> [-p|-r|-h] <file>"
            echo ""
            echo "Flags:"
            echo "  -b <backend>       : select backend ('domain', 'http', or 'mpi')"
            echo "  -p <process list>  : if the 'http' backend is selected then the list of processes that"
            echo "                       are involved, written with commas between each process name."
            echo "  -r                 : run the projected program(s)"
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
base=`basename ${file} .pir`

case $backend in
    "domain")
        dune exec -- pirc -msg-backend=domain $file &&
            ocamlfind ocamlopt -o $dir/$base.domain.exe -linkpkg -package domainslib $dir/$base.domain.ml
        if [ "$run" = "true" ]; then
            ./$dir/$base.domain.exe
        fi
        ;;
    "mpi")
        dune exec -- pirc -msg-backend=mpi $file &&
            ocamlfind ocamlopt -o $dir/$base.mpi.exe -linkpkg -package mpi $dir/$base.mpi.ml
        if [ "$run" = "true" ]; then
            mpiexec --oversubscribe ./$dir/$base.mpi.exe
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
        dune exec -- pirc -msg-backend=http $file &&
            for process in $processes; do
                OCAMLPATH=./_build/install/default/lib/ ocamlfind ocamlopt -o $dir/${base}_$process.exe -linkpkg -package http_pirc $dir/${base}_$process.ml
            done &&
            if [ "$run" = "true" ]; then
                dune exec mock_server &
                for process in $processes; do
                    $dir/${base}_$process.exe &
                done
                wait
            fi
        ;;
    *)
        echo "ERROR, invalid backend: ${backend}"
        exit 1
esac
