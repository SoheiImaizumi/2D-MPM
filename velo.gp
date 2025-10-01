set xlabel "Time"
set ylabel "Average velocity"
set grid
plot "virt_history.dat" using 1:2 with lines title "virtical velocity"
pause -1 "Press Enter to close"
