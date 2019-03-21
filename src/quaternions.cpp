// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>
#include <Eigen/Geometry> 

// via the depends attribute we tell Rcpp to create hooks for
// RcppEigen so that the build process will know what to do
//
// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::NumericVector	quatProd(const Rcpp::NumericVector & q1, 
                             const Rcpp::NumericVector & q2)
{
  if(q1.size() != 4 || q2.size() != 4){
    throw Rcpp::exception("q1 or q2 must be quaternions");
  }
  Eigen::Quaterniond qa(q1[0], q1[1], q1[2], q1[3]);
  Eigen::Quaterniond qb(q2[0], q2[1], q2[2], q2[3]);
  Eigen::Quaterniond q1q2 = qa * qb;
  return Rcpp::NumericVector::create(
    q1q2.w(), q1q2.x(), q1q2.y(), q1q2.z() );  
} 

// [[Rcpp::export]]
Rcpp::NumericVector fromAxisAngle_(const Eigen::VectorXd & axis, 
                                   const double & angle)
{
  Eigen::AngleAxisd AA(angle, axis.normalized());
  Eigen::Quaterniond q(AA);
  return Rcpp::NumericVector::create(q.w(), q.x(), q.y(), q.z());
}	


// [[Rcpp::export]]
Eigen::MatrixXd fromQuaternion_(const Rcpp::NumericVector & v)
{
  Eigen::Quaterniond q(v[0], v[1], v[2], v[3]);
  Eigen::Quaterniond qnorm = q.normalized();
  //Eigen::Quaternionf (q.w(), q.x(), q.y(), q.z());
  return qnorm.toRotationMatrix();
}	


// [[Rcpp::export]]
Rcpp::List toAxisAngle_(const Rcpp::NumericVector & v)
{
  Eigen::Quaterniond q(v[0],v[1],v[2],v[3]);
  Eigen::AngleAxisd AA(q);
  Eigen::Vector3d Axs = AA.axis();
  double angl = AA.angle();
  return Rcpp::List::create(Rcpp::Named("axis") = Axs,
                            Rcpp::Named("angle") = angl);
}	


// [[Rcpp::export]]
Rcpp::NumericVector getRotation_(const Eigen::VectorXd & a, 
                                 const Eigen::VectorXd & b)
{
  if(a.size() != 3 || b.size() != 3){
    throw Rcpp::exception("Vectors must have length 3");
  }
  //Eigen::Quaterniond q; 
  //Eigen::Quaterniond quat = q.setFromTwoVectors(a,b);
  Eigen::Quaterniond quat = Eigen::Quaterniond::FromTwoVectors(a,b);
  return Rcpp::NumericVector::create(quat.w(), quat.x(), quat.y(), quat.z());
}	


// [[Rcpp::export]]
Rcpp::NumericVector AngleAxis2quaternion(const double & angl, 
                                         const Eigen::VectorXd & axs)
{
  // faut-il normaliser l'axe ? oui
  // je l'ai déjà faite (fromAxisAngle)
  Eigen::AngleAxisd AA = Eigen::AngleAxisd(angl, axs.normalized());
  Eigen::Quaterniond quat;
  quat = Eigen::Quaterniond(AA);
  return Rcpp::NumericVector::create(quat.w(), quat.x(), quat.y(), quat.z());
}	


// [[Rcpp::export]]
Rcpp::List Matrix2AxisAngle_(const Eigen::MatrixXd & m)
{
  // m must be a valid rotation matrix
  if(m.rows() != 3 || m.cols() != 3){
    throw Rcpp::exception("`M` must be a 3x3 matrix");
  }
  Eigen::Matrix3d m3(m); // or m3 = Eigen::Matrix<double, 3, 3>(m);
  Eigen::Matrix3d tm3 = m3.transpose();
  Eigen::Matrix3d m3tm3 = m3 * tm3;
  if(!m3tm3.isIdentity(1e-3)){
    throw Rcpp::exception("`M` is not a rotation matrix");
  }
  Eigen::AngleAxisd AnAx(m3);
  double angl = AnAx.angle();
  Eigen::VectorXd axs = AnAx.axis();
  return Rcpp::List::create(Rcpp::Named("axis") = axs,
                            Rcpp::Named("angle") = angl);
}	

// [[Rcpp::export]]
Rcpp::NumericVector toQuaternion_(const Eigen::MatrixXd & m)
{
  if(m.rows() != 3 || m.cols() != 3){
    throw Rcpp::exception("`M` must be a 3x3 matrix");
  }
  Eigen::Matrix3d m3(m); 
  Eigen::Matrix3d tm3 = m3.transpose();
  Eigen::Matrix3d m3tm3 = m3 * tm3;
  if(!m3tm3.isIdentity(1e-3)){
    throw Rcpp::exception("`M` is not a rotation matrix");
  }
  Eigen::Quaterniond q(m3);
  return Rcpp::NumericVector::create(q.w(), q.x(), q.y(), q.z());
}	

// [[Rcpp::export]]
Eigen::MatrixXd AngleAxis2matrix_(const double & angl, 
                                  const Eigen::VectorXd & axs)
{
  Eigen::AngleAxisd AA = Eigen::AngleAxisd(angl, axs.normalized());
  Eigen::Matrix3d m3 = Eigen::Matrix3d(AA);
  return m3;
}	



// [[Rcpp::export]]
Rcpp::NumericVector normalized(const Rcpp::NumericVector & v)
{
  Eigen::Quaterniond q(v[0],v[1],v[2],v[3]);
  Eigen::Quaterniond qn = q.normalized();
  return Rcpp::NumericVector::create(qn.w(), qn.x(), qn.y(), qn.z());
}	

// [[Rcpp::export]]
Rcpp::NumericVector	slerp_(const Rcpp::NumericVector & q1, 
                           const Rcpp::NumericVector & q2, 
                           const double t)
{
  Eigen::Quaterniond qa(q1[0], q1[1], q1[2], q1[3]);
  Eigen::Quaterniond qb(q2[0], q2[1], q2[2], q2[3]);
  Eigen::Quaterniond q = qa.slerp(t, qb);
  return Rcpp::NumericVector::create(q.w(), q.x(), q.y(), q.z());
}
  
// [[Rcpp::export]]
Rcpp::NumericVector rversor_(){
  Eigen::Quaterniond q = Eigen::Quaterniond::UnitRandom();
  return Rcpp::NumericVector::create(q.w(), q.x(), q.y(), q.z());
}  

// [[Rcpp::export]]
Rcpp::NumericMatrix rversors_(const unsigned n){
  Rcpp::NumericMatrix out(n, 4);
  for(unsigned i=0; i<n; i++){
    Eigen::Quaterniond q = Eigen::Quaterniond::UnitRandom();
    out(i,0) = q.w();
    out(i,1) = q.x();
    out(i,2) = q.y();
    out(i,3) = q.z();
  }
  return out;
}  

//  rqs
//  q[1] = cos(angle/2)
//  sqrt(crossprod(q[2:4])) = sin(angle/2)