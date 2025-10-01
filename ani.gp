set terminal x11
set xrange [0.0:1.0]
set yrange [1.0:2.0]
set size square      

do for [t = 1:3000] {
    fname = sprintf("points_%05d.dat", t)
        plot fname using 1:2 with points pt 7 ps 2 lc rgb "red" title sprintf("Step %d", t)
	    pause 0.01
	    }
