#include <Rcpp.h>
#include <vector>
// #include <boost/math/special_functions/beta.hpp>

// this doesn't work,  added export PKG_CXXFLAGS=-std=c++11 to bash
// [[Rcpp::plugins("cpp11")]]

using namespace Rcpp;
using std::vector;

// from: http://stackoverflow.com/a/12399290/453735
vector<size_t> order(const NumericVector &v) {
  // initialize original index locations
  vector<size_t> idx(v.length());
  for (size_t i = 0; i != idx.size(); ++i) idx[i] = i;
  // sort indexes based on comparing values in v
  sort(idx.begin(), idx.end(),
       [&v](size_t i1, size_t i2) {
         return v[i1] < v[i2];
       });
  return idx;
}

// double w_ibeta(double& p, NumericVector& r) {
//   return boost::math::ibeta(r[0], r[1], p);
// }

double w_pow(double& p, NumericVector& r){

  if(r.length() > 1){
    double H = 1 - pow(1 - p, r[1]);
    double L = pow(p, r[0]);
    return L*p + H*(1 - p);
  } else {
    return pow(p, r[0]);
  }
}


// [[Rcpp::export]]
NumericVector c_rdmean_pow(NumericVector x, NumericVector w, NumericVector p) {
  if (x.length() != w.length()){
    Rf_error("Input vectors 'x' and 'w' must be of the same length.");
  }
  double wsum =  std::accumulate(w.begin(), w.end(), 0.0);
  std::transform(w.begin(), w.end(), w.begin(),
                 [&wsum](double x) {return x/wsum;});
  double out = 0, hacc = 0, lacc = 0;
  double pred = 0;
  int done = 0;
  auto ix = order(x);
  for (auto i : ix ){
    // Rprintf("%d:%f \n",  i,  x[i]);
    done = 1;
      hacc += w[i];
      if(!NumericVector::is_na(x[i])){
        // order algorithm roughly preserves orders of NA's
        // not sure if this is the right approach
        out += x[i]*(w_pow(hacc, p) - w_pow(lacc, p));
      }
      lacc = hacc;
  }
  if (!done)
    out = NA_REAL;
  return NumericVector::create(out);
}

// // [[Rcpp::export]]
// int useAuto() {
//   auto val = 42;		// val will be of type int
//   return val;
// }
