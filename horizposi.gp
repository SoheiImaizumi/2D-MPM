set xlabel "Time"                                                                                                              
set ylabel "Average Horizonal position"
set grid
plot "virt_history.dat" using 1:3 with lines title "horizonal position"                                     
pause -1 "Press Enter to close"
