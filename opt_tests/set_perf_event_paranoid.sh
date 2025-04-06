# https://askubuntu.com/questions/1471162/how-to-change-kernel-perf-event-paranoid-settings
sudo sysctl kernel.perf_event_paranoid=-1
sudo sysctl kernel.kptr_restrict=0
