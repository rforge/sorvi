# Copyright (C) 2012 Louhos (louhos.github.com)
# Contact: louhos.github.com/contact.html
# All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License:
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script was implemented with soRvi version 0.1.72

# Plot borders for municipalities (kunta) and provinces (maakunta)
library(sorvi)
data(MML)
kunnat <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]
PlotShape(kunnat, varname = "Maakunta")

