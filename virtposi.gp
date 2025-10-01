set xlabel "Time"
set ylabel "Average Virtical Position"
set grid
plot "virt_history.dat" using 1:4 with lines title "virtical velocity"
pause -1 "Press Enter to close"
