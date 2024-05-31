

# simulate powerposing study with respect to effect size
pwr::pwr.t.test(n = 21,sig.level = 0.05, power = 0.8,
           type = "two.sample",
           alternative = "two.sided")


# simulate powerposing study with respect to N
pwr::pwr.t.test(power = .8,sig.level = 0.05, d = 0.2,
                type = "two.sample",
                alternative = "two.sided")
