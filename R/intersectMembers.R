##     matchMembers.R Subset two or more grids to the common members
##
##     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Subset two or more grids to the common members.
#' @description Subset two or more grids to the common members.
#' @param ... Grids.
#' @return input grids are updated in the global environment.
#' @author M. Iturbide
#' @export


intersectMembers <- function(...){
  grids <- list(...) %>% lapply(redim)
  grids <- list(hist.y, fut.ens) %>% lapply(redim)
  grids.members <- lapply(grids, "[[", "Members")
  members <- do.call("intersect", grids.members)
  if (is.character(members) & length(members) > 0) {
    ind <- lapply(grids.members, function(m){
      lapply(members, function(i) grep(i, m)) %>% unlist
    })
    out <- lapply(1:length(grids), function(x) subsetGrid(grids[[x]], members = ind[[x]]))
    nmes <- as.character(as.list(substitute(list(...)))[-1L])
    if (length(nmes) < length(grids)) nmes <- paste0(nmes, 1:length(grids))
    names(out) <- nmes
    lapply(1:length(out), function(en) .GlobalEnv[[names(out)[en]]] <- out[[en]])
  } else {
    stop("Check input grids. Ensure the member dimension exists.")
  }
}

#end