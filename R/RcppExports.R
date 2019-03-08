# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

quatProd <- function(q1, q2) {
    .Call('_RcppQuaternions_quatProd', PACKAGE = 'RcppQuaternions', q1, q2)
}

fromAxisAngle <- function(axis, angle) {
    .Call('_RcppQuaternions_fromAxisAngle', PACKAGE = 'RcppQuaternions', axis, angle)
}

fromQuaternion <- function(v) {
    .Call('_RcppQuaternions_fromQuaternion', PACKAGE = 'RcppQuaternions', v)
}

toAxisAngle <- function(v) {
    .Call('_RcppQuaternions_toAxisAngle', PACKAGE = 'RcppQuaternions', v)
}

getRotation <- function(a, b) {
    .Call('_RcppQuaternions_getRotation', PACKAGE = 'RcppQuaternions', a, b)
}

AngleAxis2quaternion <- function(angl, axs) {
    .Call('_RcppQuaternions_AngleAxis2quaternion', PACKAGE = 'RcppQuaternions', angl, axs)
}

Matrix2AxisAngle <- function(m) {
    .Call('_RcppQuaternions_Matrix2AxisAngle', PACKAGE = 'RcppQuaternions', m)
}

toQuaternion <- function(m) {
    .Call('_RcppQuaternions_toQuaternion', PACKAGE = 'RcppQuaternions', m)
}

AngleAxis2matrix <- function(angl, axs) {
    .Call('_RcppQuaternions_AngleAxis2matrix', PACKAGE = 'RcppQuaternions', angl, axs)
}

normalized <- function(v) {
    .Call('_RcppQuaternions_normalized', PACKAGE = 'RcppQuaternions', v)
}

slerp_ <- function(q1, q2, t) {
    .Call('_RcppQuaternions_slerp_', PACKAGE = 'RcppQuaternions', q1, q2, t)
}

rversor_ <- function() {
    .Call('_RcppQuaternions_rversor_', PACKAGE = 'RcppQuaternions')
}

rversors_ <- function(n) {
    .Call('_RcppQuaternions_rversors_', PACKAGE = 'RcppQuaternions', n)
}
