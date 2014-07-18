## 25 Jul 2013: Update slope for viral load increase for women who have reached
## pre-ART levels after stopping ART.

## The steps to follow are:
## a. If time since cessation of ART is greater than the time for return to pre-ART VIral load, then the slope paramter for viral load trajectory will have to be updated.
## This updated value will depend on the time since infection.

time.since.art.cessation <- nw%v%"time.since.art.cessation"
vl.art.traj.slope <- nw%v%"vl.art.traj.slope"

female.id.curr <- nwmodes(nw, 2)

for (i in 1:length(female.id.curr)){

  if(time.since.art.cessation[female.id.curr[i]] >= sc.art.postcess.ret.bl){
    if (time.since.infection[female.id.curr[i] %in% 
    



