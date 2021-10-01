#' Find midpoints
#' https://stackoverflow.com/questions/49327247/add-labels-to-the-center-of-a-geom-curve-line-ggplot
#' This function had not been touched from the copied version from
#' stack overflow. Not exporting since we don't have full documentation
#' of author, etc.

#' @noRd

calc_control_points <- function(x1, y1, x2, y2, curvature, angle, ncp,
                              debug=FALSE) {
  # Negative curvature means curve to the left
  # Positive curvature means curve to the right
  # Special case curvature = 0 (straight line) has been handled
  xm <- (x1 + x2)/2
  ym <- (y1 + y2)/2
  dx <- x2 - x1
  dy <- y2 - y1
  slope <- dy/dx

  # Calculate "corner" of region to produce control points in
  # (depends on 'angle', which MUST lie between 0 and 180)
  # Find by rotating start point by angle around mid point
  if (is.null(angle)) {
    # Calculate angle automatically
    angle <- ifelse(slope < 0,
                    2*atan(abs(slope)),
                    2*atan(1/slope))
  } else {
    angle <- angle/180*pi
  }
  sina <- sin(angle)
  cosa <- cos(angle)
  # FIXME:  special case of vertical or horizontal line ?
  cornerx <- xm + (x1 - xm)*cosa - (y1 - ym)*sina
  cornery <- ym + (y1 - ym)*cosa + (x1 - xm)*sina

  # Debugging
  if (debug) {
    grid.points(cornerx, cornery, default.units="inches",
                pch=16, size=unit(3, "mm"),
                gp=gpar(col="grey"))
  }

  # Calculate angle to rotate region by to align it with x/y axes
  beta <- -atan((cornery - y1)/(cornerx - x1))
  sinb <- sin(beta)
  cosb <- cos(beta)
  # Rotate end point about start point to align region with x/y axes
  newx2 <- x1 + dx*cosb - dy*sinb
  newy2 <- y1 + dy*cosb + dx*sinb

  # Calculate x-scale factor to make region "square"
  # FIXME:  special case of vertical or horizontal line ?
  scalex <- (newy2 - y1)/(newx2 - x1)
  # Scale end points to make region "square"
  newx1 <- x1*scalex
  newx2 <- newx2*scalex

  # Calculate the origin in the "square" region
  # (for rotating start point to produce control points)
  # (depends on 'curvature')
  # 'origin' calculated from 'curvature'
  ratio <- 2*(sin(atan(curvature))^2)
  origin <- curvature - curvature/ratio
  # 'hand' also calculated from 'curvature'
  if (curvature > 0)
    hand <- "right"
  else
    hand <- "left"
  oxy <- calc_origin(newx1, y1, newx2, newy2, origin, hand)
  ox <- oxy$x
  oy <- oxy$y

  # Calculate control points
  # Direction of rotation depends on 'hand'
  dir <- switch(hand,
                left=-1,
                right=1)
  # Angle of rotation depends on location of origin
  maxtheta <- pi + sign(origin*dir)*2*atan(abs(origin))
  theta <- seq(0, dir*maxtheta,
               dir*maxtheta/(ncp + 1))[c(-1, -(ncp + 2))]
  costheta <- cos(theta)
  sintheta <- sin(theta)
  # May have BOTH multiple end points AND multiple
  # control points to generate (per set of end points)
  # Generate consecutive sets of control points by performing
  # matrix multiplication
  cpx <- ox + ((newx1 - ox) %*% t(costheta)) -
    ((y1 - oy) %*% t(sintheta))
  cpy <- oy + ((y1 - oy) %*% t(costheta)) +
    ((newx1 - ox) %*% t(sintheta))

  # Reverse transformations (scaling and rotation) to
  # produce control points in the original space
  cpx <- cpx/scalex
  sinnb <- sin(-beta)
  cosnb <- cos(-beta)
  finalcpx <- x1 + (cpx - x1)*cosnb - (cpy - y1)*sinnb
  finalcpy <- y1 + (cpy - y1)*cosnb + (cpx - x1)*sinnb

  # Debugging
  if (debug) {
    ox <- ox/scalex
    fox <- x1 + (ox - x1)*cosnb - (oy - y1)*sinnb
    foy <- y1 + (oy - y1)*cosnb + (ox - x1)*sinnb
    grid.points(fox, foy, default.units="inches",
                pch=16, size=unit(1, "mm"),
                gp=gpar(col="grey"))
    grid.circle(fox, foy, sqrt((ox - x1)^2 + (oy - y1)^2),
                default.units="inches",
                gp=gpar(col="grey"))
  }

  list(x=as.numeric(t(finalcpx)), y=as.numeric(t(finalcpy)))
}




#' Calc origin
#' https://stackoverflow.com/questions/49327247/add-labels-to-the-center-of-a-geom-curve-line-ggplot
#' @noRd

calc_origin <- function(x1, y1, x2, y2, origin, hand) {
  # Positive origin means origin to the "right"
  # Negative origin means origin to the "left"
  xm <- (x1 + x2)/2
  ym <- (y1 + y2)/2
  dx <- x2 - x1
  dy <- y2 - y1
  slope <- dy/dx
  oslope <- -1/slope
  # The origin is a point somewhere along the line between
  # the end points, rotated by 90 (or -90) degrees
  # Two special cases:
  # If slope is non-finite then the end points lie on a vertical line, so
  # the origin lies along a horizontal line (oslope = 0)
  # If oslope is non-finite then the end points lie on a horizontal line,
  # so the origin lies along a vertical line (oslope = Inf)
  tmpox <- ifelse(!is.finite(slope),
                  xm,
                  ifelse(!is.finite(oslope),
                         xm + origin*(x2 - x1)/2,
                         xm + origin*(x2 - x1)/2))
  tmpoy <- ifelse(!is.finite(slope),
                  ym + origin*(y2 - y1)/2,
                  ifelse(!is.finite(oslope),
                         ym,
                         ym + origin*(y2 - y1)/2))
  # ALWAYS rotate by -90 about midpoint between end points
  # Actually no need for "hand" because "origin" also
  # encodes direction
  # sintheta <- switch(hand, left=-1, right=1)
  sintheta <- -1
  ox <- xm - (tmpoy - ym)*sintheta
  oy <- ym + (tmpox - xm)*sintheta

  list(x=ox, y=oy)
}
