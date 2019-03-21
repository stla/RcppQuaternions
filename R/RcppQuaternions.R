#' Quaternion product
#' @description Product of two quaternions.
#' @param q1,q2 quaternions
#' @return A quaternion, the product of \code{q1} and \code{q2}.
qprod <- function(q1, q2){
  quatProd(q1, q2)
}

#' Quaternion from axis and angle
#' @description Quaternion representation of a rotation given by axis and angle.
#' @param axis rotation axis, a vector of length 3
#' @param angle rotation angle in radians
#' @return A (normalized) quaternion.
#' @examples  
#' q <- fromAxisAngle(c(1,2,4), 2)
#' toAxisAngle(q)
fromAxisAngle <- function(axis, angle){
  fromAxisAngle_(axis, angle)
}

#' Rotation matrix from quaternion
#' @description Rotation matrix corresponding to a quaternion.
#' @param q quaternion
#' @return The rotation matrix corresponding to \code{q}.
#' @examples 
#' R <- fromQuaternion(c(1,2,3,4))
#' RtoQuaternion(R)
fromQuaternion <- function(q){
  fromQuaternion_(q)
}

#' Axis and angle of the rotation corresponding to a quaternion
#' @description Rotation corresponding to a quaternion, given by axis and angle.
#' @param q quaternion
#' @return A list, the axis and the angle.
#' @examples 
#' aa <- toAxisAngle(c(1,2,3,4))
#' fromAxisAngle(aa$axis, aa$angle)
toAxisAngle <- function(q){
  toAxisAngle_(q)
}

#' Quaternion representation of a rotation between two vectors
#' @description Quaternion representation of a rotation transforming a vector 
#' to another one
#' @param u,v vectors
#' @return A quaternion, whose corresponding rotation transforms \code{u} 
#' to \code{v}.
#' @examples 
#' u <- c(1,2,3)
#' v <- c(3,2,1)
#' q <- getRotation(u,v)
#' R <- fromQuaternion(q)
#' R %*% u
getRotation <- function(u,v){
  getRotation_(u, v)
}

#' Axis and angle of a rotation 
#' @description Axis and angle of a rotation given by its matrix.
#' @param R rotation matrix
#' @return A list, the axis and the angle.
#' @examples 
#' theta <- 2
#' R <- rbind(c(cos(theta), sin(theta), 0), 
#' c(-sin(theta), cos(theta), 0), 
#' c(0, 0, 1))
#' RtoAxisAngle(R)
RtoAxisAngle <- function(R){
  Matrix2AxisAngle_(R)
}

#' Quaternion representation of a rotation matrix
#' @description Quaternion corresponding to a rotation matrix.
#' @param R rotation matrix
#' @return The (normalized) quaternion representing \code{R}.
#' @examples 
#' R <- fromQuaternion(c(1,2,3,4))
#' RtoQuaternion(R)
RtoQuaternion <- function(R){
  toQuaternion_(R)
}

#' Rotation matrix from angle and axis 
#' @description Rotation matrix from rotation angle and axis.
#' @param angle the angle of rotation, in radians
#' @param axis the axis of rotation
#' @return The rotation matrix.
#' @examples 
#' R <- AxisAngle(c(1,2,3), 2)
#' RtoQuaternion(R)
#' fromAxisAngle(c(1,2,3), 2)
AxisAngle <- function(axis, angle){
  AngleAxis2matrix_(angle, axis)
}

