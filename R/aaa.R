# "fix" R CMD check notes
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", ".__adj", ".__by_ht", ".__grp", ".__ht", ".__len", ".__max",
      ".__mid", ".__min", ".__n", ".__tmp", ".__y", "PANEL", "group",
      "grp_max", "grp_min", "x", "y", "ylower", "ymax", "ymin", "yupper")
  )
}

if (getRversion() >= "3.1.0") {
  utils::suppressForeignCheck(
    c(
      ".", ".__adj", ".__by_ht", ".__grp", ".__ht", ".__len", ".__max",
      ".__mid", ".__min", ".__n", ".__tmp", ".__y", "PANEL", "group",
      "grp_max", "grp_min", "x", "y", "ylower", "ymax", "ymin", "yupper")
  )
}
