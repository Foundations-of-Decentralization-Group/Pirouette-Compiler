set style line 1 lc rgb "gray"
set style line 2 lc rgb "blue"

set style fill solid
set boxwidth 0.5
set key outside

set xlabel "Number of Iterations;% difference"
set ylabel "Time taken in seconds"

set xtics ("50;0.11-U" 0.25, "75;1.35-U" 1.75, "100;1.08-U" 3.25, "125;0.65-U" 4.75, "150;0.26-U" 6.25)

plot 'Graph_Info_Plot.txt' every 2    using 1:2 title "Unoptimized" with boxes ls 1,\
     'Graph_Info_Plot.txt' every 2::1 using 1:2 title "Optimized" with boxes ls 2
