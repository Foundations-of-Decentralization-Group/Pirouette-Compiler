set style line 1 lc rgb "gray"
set style line 2 lc rgb "blue"

set style fill solid
set boxwidth 0.5
set key outside
set autoscale

set title "MPI - 7 participants, Unopt vs Opt"
set xlabel "Number of Iterations"
set yrange [0:70]

set xtics ("1x10^6" 0.25, "2x10^6" 1.75, "1x10^7" 3.25)

plot 'Graph_Info_Variance_Plot.txt' every 2    using 1:2 title "Unoptimized" with boxes ls 1,\
     'Graph_Info_Variance_Plot.txt' every 2::1 using 1:2 title "Optimized" with boxes ls 2
