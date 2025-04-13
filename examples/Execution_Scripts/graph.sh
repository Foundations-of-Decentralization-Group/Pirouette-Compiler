set style line 1 lc rgb "gray"
set style line 2 lc rgb "black"

set style fill solid
set boxwidth 0.5
set key outside

set xlabel "Number of Iterations"
set ylabel "Time taken in seconds"

set xtics ("50" 0.25, "75" 1.75, "100" 3.25, "125" 4.75)

plot 'Graph_Info.txt' every 2    using 1:2 title "Unoptimized" with boxes ls 1,\
     'Graph_Info.txt' every 2::1 using 1:2 title "Optimized" with boxes ls 2
