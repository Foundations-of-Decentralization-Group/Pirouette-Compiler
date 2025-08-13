#! /bin/bash
# Record system information for the current node into the directory specified in $1
#   $1 : directory to put current node info

host_name=$(hostname -s)
node_info_dir=$1
mkdir -p $node_info_dir/$host_name

function rec_info
# Record system information from a specific command
#   $1 : command to run
#   $2 : file to save to in $node_info_dir
{
    $1 > "$node_info_dir/$host_name/$2"
}

# Record the system info of the current node
rec_info "lscpu"                   lscpu.txt
rec_info "cat /proc/cpuinfo"       cpuinfo.txt
rec_info "uname -a"                uname-a.txt
rec_info "lsb_release -a"          lsb_release-a.txt
rec_info "cat /etc/os-release"     os-release.txt
rec_info "numactl --show"          numa-show.txt
rec_info "numactl --hardware"      numa-hardware.txt
