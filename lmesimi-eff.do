use /tmp/stage2i
set linesize 200

mixed y x z [pw=w2] || g: z, cov(uns) pweight(w1) pwscale(gk) iterate(100)
log using /tmp/stage2i, nomsg replace
matrix list e(b), noblank nonames noheader

log off
mixed y x z [pw=w2*w1] || g: z, cov(uns) pweight(w1) pwscale(size) iterate(100)
log on
matrix list e(b), noblank nonames noheader

log off
mixed y x z [pw=w2*w1] || g: z, cov(uns) pweight(w1)  iterate(100)
log on
matrix list e(b), noblank nonames noheader


log close
exit
