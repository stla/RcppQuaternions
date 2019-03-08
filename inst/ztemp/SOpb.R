library(pracma)

# Build quaternion between two vectors
# This comes from http://lolengine.net/blog/2013/09/18/beautiful-maths-quaternion-from-vectors, towards the bottom
quatFromTwoVectors <- function(vStart, vEnd) {
  vStart <- vStart / norm(vStart, "2")
  vEnd <- vEnd / norm(vEnd, "2")
  
  s <- sqrt(2 + 2 * dot(vStart, vEnd))
  w <- (1/s) * cross(vStart, vEnd)
  quat <- c(s/2, w)
  return(quat)
}

# Build rotation matrix from given quaternion NORMALIZED
rotationMatrixFromQuaternion <- function(quat) {
  qCross <- matrix(c(c(0, quat[4], -quat[3]), c(-quat[4], 0, quat[2]), c(quat[3], -quat[2], 0)), ncol = 3)
  
  rotMat <- (quat[1]^2 - norm(quat[2:4], "2")^2) * diag(1,3) + 2 * quat[2:4] %*% t(quat[2:4]) + 2 * quat[1] * qCross
  return(rotMat)
}

quaternionMultiplication <- function(q1, q2) {
  s <- q1[1] * q2[1] - dot(q1[2:4], q2[2:4])
  w <- q1[1] * q2[2:4] + q2[1] * q1[2:4] + cross(q1[2:4], q2[2:4])
  quat <- c(s,w)
  quat <- quat / norm(quat, "2")
  
  return(quat)
}

quaternionInverse <- function(quat) {
  quat <- c(quat[1], -quat[2:4])
  quat <- quat / norm(quat, "2")
  return(quat)
}

# The following quaternion (s, x, y, z) rotation corresponds to the ZYX Euler angles (12 deg, -23 deg, 34 deg)
# ASSUME THIS IS UNKNOWN BEFOREHAND
rotationFrame1ToFrame2 <- c(0.9258802, 0.1559245, -0.1596644, 0.3048618)

# Inverse of prior quaternion
# ASSUME THIS IS UNKNOWN BEFOREHAND
rotationFrame2ToFrame1 <- c(0.9258802, -0.1559245, 0.1596644, -0.3048618)

# This rotation matrix agrees with the output from https://www.andre-gaschler.com/rotationconverter/ using the above Euler quaternion
# ASSUME THIS IS UNKNOWN BEFOREHAND
rotMat <- rotationMatrixFromQuaternion(rotationFrame1ToFrame2) # j'ai changé

getRotation2 <- function(U,V){
  ma <- sqrt(c(crossprod(U)))
  mb <- sqrt(c(crossprod(V))) # ?? no sense if ma /= mb ...
  d <- c(tcrossprod(U, t(V)))
  c <- sqrt(ma*mb+d)
  ma2 <- sqrt(2)*ma
  r <- 1/ma2/c
  W <- geometry::extprod3d(U,V)
  c(c/ma2, r*W)
}
# Reference vectors in frames 1 (global, known) & 2 (local, known)
vectorFrame1 <- c(1,0,0) # (1:3)/sqrt(14)
vectorFrame2 <- as.vector(rotMat %*% vectorFrame1)

# Estimate rotation between frames 1 & 2 using quatFromTwoVectors
rotationFrame2ToFrame1.estimate <- quatFromTwoVectors(vectorFrame1, vectorFrame2)

# The quaternion difference between estimated and true rotations is non-trivial. 
# **Why?**
rotationDifference <- quaternionMultiplication(rotationFrame2ToFrame1.estimate, 
                                               quaternionInverse(rotationFrame2ToFrame1))
rotationMatrixFromQuaternion(rotationDifference)

# Furthermore, why is vectorFrame1 the rotation axis for rotationDifference? (It's an eigenvector with eigenvalue of +1)
rotationMatrixFromQuaternion(rotationDifference) %*% vectorFrame1 - vectorFrame1    # Should equal origin to machine precision
eigen(rotationMatrixFromQuaternion(rotationDifference))   # The only real eigenvector normalizes to the unit version of vectorFrame1

# If I could determine the rotation angle of rotationDifference (without already knowing the true rotation), 
# I could "subtract out" the rotation difference and calculate the true rotation
trueDifferenceAngle <- acos(rotationDifference[1]) * 2
vectorFrame1.unit <- vectorFrame1 / norm(vectorFrame1, "2")
rotationDifference.AxisAngleForm <- c(cos(trueDifferenceAngle/2), sin(trueDifferenceAngle/2) * vectorFrame1.unit)
rotationDifference - rotationDifference.AxisAngleForm   # Should equal origin to machine precision

quaternionMultiplication(quaternionInverse(rotationDifference.AxisAngleForm), rotationFrame2ToFrame1.estimate) - rotationFrame2ToFrame1   # Should equal origin to machine precision