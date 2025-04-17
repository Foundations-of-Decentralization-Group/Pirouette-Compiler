set style line 1 lc rgb "gray"
set style line 2 lc rgb "blue"

set style fill solid
set boxwidth 0.5
set key outside

set xlabel "Number of Iterations;% difference"
set ylabel "Time taken in seconds"

set xtics ("50;0.31-O" 0.25, "75;0.18-U" 1.75, "100;0.12-U" 3.25, "125;0.19-U" 4.75, "150;0.17-U" 6.25)

plot 'Graph_Info_Plot.txt' every 2    using 1:2 title "Unoptimized" with boxes ls 1,\
     'Graph_Info_Plot.txt' every 2::1 using 1:2 title "Optimized" with boxes ls 2
