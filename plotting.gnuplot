unset key
set cbtics "out"
set xlabel "step"
set ylabel "τ(step)"
set output "radius-both.png"
plot "radius-slow.dat" with lines linecolor rgb "red", "radius.dat" with lines linecolor rgb "green"
