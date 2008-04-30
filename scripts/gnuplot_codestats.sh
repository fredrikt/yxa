#!/bin/sh

CODELINES_YRANGE="90000"

if [ -f official-yxa.data -a -f yxa-not-fork.data ]; then

gnuplot <<-EOF
    set xdata time
    set timefmt "%Y-%m-%d"
    set format x "%Y-%m"
    set timestamp "generated on %Y-%m-%d"
    
    set terminal png
    set output "combined-code-lines.png"
    set title "YXA lines of code, SU and official numbers combined"
    set yrange [ 0 : $CODELINES_YRANGE ]

    # key is legend
    #set key box
    set key spacing 1.5
    set key left
 
    plot 'official-yxa.data' using 1:2 title 'code lines' smooth csplines, \
         'official-yxa.data' using 1:3 title 'comment lines' smooth csplines, \
         'official-yxa.data' using 1:4 title 'test code lines' smooth csplines, \
         'official-yxa.data' using 1:6 title 'total lines' smooth csplines, \
         \
         'yxa-not-fork.data' using 1:2 title 'SU code lines' smooth csplines, \
         'yxa-not-fork.data' using 1:3 title 'SU comment lines' smooth csplines, \
         'yxa-not-fork.data' using 1:4 title 'SU test code lines' smooth csplines, \
         'yxa-not-fork.data' using 1:6 title 'SU total lines' smooth csplines
         
         
EOF

else
    echo "yxa-not-fork.data or official-yxa.data not found"
fi

if [ -f official-yxa.data ]; then

gnuplot <<-EOF
    set xdata time
    set timefmt "%Y-%m-%d"
    set format x "%Y-%m"
    set timestamp "generated on %Y-%m-%d"

    set terminal png
    set output "code-lines.png"
    set title "Official YXA lines of code"
    set yrange [ 0 : $CODELINES_YRANGE ]

    # key is legend
    #set key box
    set key spacing 1.5
    set key left
 
    plot 'official-yxa.data' using 1:2 title 'code lines' smooth csplines, \
         'official-yxa.data' using 1:3 title 'comment lines' smooth csplines, \
         'official-yxa.data' using 1:4 title 'test code lines' smooth csplines, \
         'official-yxa.data' using 1:6 title 'total lines' smooth csplines
EOF

else
    echo "official-yxa.data not found"
fi

if [ -f yxa-not-fork.data ]; then

gnuplot <<-EOF
    set xdata time
    set timefmt "%Y-%m-%d"
    set format x "%Y-%m"
    set timestamp "generated on %Y-%m-%d"

    set terminal png
    set output "su-code-lines.png"
    set title "SU\'s internal YXA lines of code"
    set yrange [ 0 : 60000 ]

    # key is legend
    #set key box
    set key spacing 1.5
    set key left
 
    plot 'yxa-not-fork.data' using 1:2 title 'SU code lines' smooth csplines, \
         'yxa-not-fork.data' using 1:3 title 'SU comment lines' smooth csplines, \
         'yxa-not-fork.data' using 1:4 title 'SU test code lines' smooth csplines, \
         'yxa-not-fork.data' using 1:6 title 'SU total lines' smooth csplines
EOF

else
    echo "yxa-not-fork.data not found"
fi

if [ -f snapshot-size.txt ]; then

gnuplot <<-EOF
    set xdata time
    set timefmt "%Y-%m-%d"
    set format x "%Y-%m"
    set timestamp "generated on %Y-%m-%d"

    set terminal png
    set output "snapshot-size.png"
    set title "YXA snapshots compressed size"
    set yrange [ 0 : 600000 ]

    # key is legend
    #set key box
    set key spacing 1.5
    set key left
 
    plot 'snapshot-size.txt' using 1:2 t 'Bytes' with lines
EOF

else
    echo "snapshot-size.txt not found"
fi


# autoscale max     set yrange [ 0 : ]

