set style fill solid
set style line 1 lc rgb "blue"
set boxwidth 0.5
set key outside
set autoscale
set yrange [0.7:1]
set title "Speedup - 7 participants, Unopt vs Opt"
set xlabel "Speedup (Unoptimized time/Optimized time)"

plot "Graph_Speedup_Info.txt" using 1:3:xtic(2) with boxes notitle ls 1
