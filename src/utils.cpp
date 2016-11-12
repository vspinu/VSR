#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;

//' @export
// [[Rcpp::export]]
NumericVector c_fill_na(NumericVector& x) {
  if (x.length() == 0) {
    return x;
  } else {
    NumericVector out(x.length());
    double prev = x[0];
    out[0] = prev;
    for (size_t i = 1; i < x.length(); i++){
      if (!ISNA(x[i])) prev = x[i];
      out[i] = prev;
    }
    return out;
  }
}