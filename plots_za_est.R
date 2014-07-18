## 12 Nov 2013: examine starting network structure for South Africa
   load("estimation_za_phase2_entr18_d8d.RData")

library(ergm)
library(tergm)
library(networkDynamic)

list.vertex.attributes(n0)

table(n0%v%"sex", useNA="always")
summary(n0%v%"age")

table(n0%v%"age.cat", useNA="always")
mixingmatrix(n0, "age.cat")$matrix/
  sum(mixingmatrix(n0, "age.cat")$matrix)

table(n0%v%"inf.status", useNA="always")/
  sum(table(n0%v%"inf.status", useNA="always"))

table(n0%v%"art.status", useNA="always")/
  sum(table(n0%v%"art.status", useNA="always"))

table(n0%v%"dur.inf.by.age", useNA="always")/
  sum(table(n0%v%"dur.inf.by.age", useNA="always"))

table(n0%v%"art.covered", useNA="always")/
  sum(table(n0%v%"art.covered", useNA="always"))

table(n0%v%"art.covered", useNA="no")/
  sum(table(n0%v%"art.covered", useNA="no"))

table(n0%v%"preg.covered", useNA="always")/
  sum(table(n0%v%"preg.covered", useNA="always"))

table(n0%v%"preg.covered", useNA="no")/
  sum(table(n0%v%"preg.covered", useNA="no"))

table(n0%v%"curr.pregnancy.status", useNA="no")/
  sum(table(n0%v%"curr.pregnancy.status", useNA="no"))

table(n0%v%"curr.pregnancy.status", useNA="always")/
  sum(table(n0%v%"curr.preg.status", useNA="always"))



