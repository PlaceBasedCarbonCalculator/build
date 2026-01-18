bivariate_categories = function(x,y, zeroNA = FALSE){

  if(zeroNA){
    x[x == 0] = NA
    y[y == 0] = NA
    qx <- quantile(x, probs = seq(0.2, 0.8, by = 0.2), type = 7, na.rm = TRUE)
    qy <- quantile(y, probs = seq(0.2, 0.8, by = 0.2), type = 7, na.rm = TRUE)
  } else {
    qx <- quantile(x, probs = seq(0.2, 0.8, by = 0.2), type = 7, na.rm = TRUE)
    qy <- quantile(y, probs = seq(0.2, 0.8, by = 0.2), type = 7, na.rm = TRUE)
  }

  dat = data.frame(x = x, y = y)

  dat = dat |>
    dplyr::mutate(
      xq = dplyr::case_when(
        x < qx[1] ~ 10L,
        dplyr::between(x, qx[1], qx[2]) ~ 20L,
        dplyr::between(x, qx[2], qx[3]) ~ 30L,
        dplyr::between(x, qx[3], qx[4]) ~ 40L,
        x > qx[4] ~ 50L
      ),
      yq = dplyr::case_when(
        y < qy[1] ~ 1L,
        dplyr::between(y, qy[1], qy[2]) ~ 2L,
        dplyr::between(y, qy[2], qy[3]) ~ 3L,
        dplyr::between(y, qy[3], qy[4]) ~ 4L,
        y > qy[4] ~ 5L
      )
    )

  dat$bivariate_code = dat$xq + dat$yq

  dat$bivariate_code

}


