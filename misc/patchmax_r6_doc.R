# example plot showing objective distance modifier
sdw_ex = 10
plot(seq(0,1,by=0.01), (1-seq(0,1,by=0.01))^sdw, type='l',
     xlab='Objective (0-1)', ylab='Distance modifier (0-1)')

# example plot showing objective distance modifier
# e.g., distance cost is sdw-times of the highest
sdw = 10
x = seq(0,1,by=0.01)

s-x*sdw+x
plot(x, (1-x)*(sdw-1)+1, type='l',
     xlab='Objective (0-1)', ylab='Distance multiplier (0-1)')


# example plot showing exclusion modifier
# e.g., distance cost in excluded areas is sdw-times that of non-excluded areas

x*sdw-x+1
plot(x, x*(sdw-1)+1, type='l',
     xlab='Excluded (0 or 1)', ylab='Distance multiplier (0-1)')
