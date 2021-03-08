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
