#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;

//' @export
// [[Rcpp::export]]
NumericVector c_cumema(NumericVector& X, NumericVector& days, double theta){
  if (theta == 0) {
    return X;
  }

  int N = X.length();
  NumericVector out(N);

  if (N > 0) {
    out[0] = X[0];

    for(int i = 1; i < N; i++){
      int prev = i - 1;
      double delta = days[i] - days[prev];
      if( delta < 0) Rf_error("days argument is not increasing");
      double W = exp(-delta/theta);
      out[i] = W*out[prev] + X[i];
    }
  }
  return out;
}

//' @export
// [[Rcpp::export]]
NumericVector c_ema(NumericVector& X, NumericVector& days, double theta){
  if (theta == 0) {
    return X;
  }

  int N = X.length();
  NumericVector out(N);

  if (N > 0) {
    out[0] = X[0];
    for (int i = 1; i < N; i++) {
      int prev = i - 1;
      double edelta = days[i] - days[prev];
      if (edelta < 0) Rf_error("days argument is not increasing");
      double W = exp(-edelta/theta);
      out[i] = W*out[prev] + (1.0 - W)*X[i];
    }
  }
  return out;
}

//' @export
// [[Rcpp::export]]
NumericVector c_ema_lin(NumericVector& X, NumericVector& days, double theta){
  // linear interpolation
  // 3.3 in VSR/docs/ts_alg.pdf and  http://oroboro.com/irregular-ema/

  if (theta == 0) {
    return X;
  }

  int N = X.length();
  NumericVector out(N);

  if (N > 0) {
    out[0] = X[0];

    for(int i = 1; i < N; i++){
      int prev = i - 1;
      double a = (days[i] - days[prev])/theta;
      double W = exp( -a );
      double V = ( 1 - W ) / a;
      out[i] = ( W * out[prev] ) + (( V - W ) * X[prev] ) + (( 1.0 - V ) * X[i] );
    }
    
  }
  return out;
}

//' @export
// [[Rcpp::export]]
NumericVector c_ediversity(IntegerVector& X, int N, double theta){
  ////// NumericVector c_ediversity(IntegerVector& X, int N, NumericVector& days, double theta){
  // X: ints from 1 to N inclusively
  // days: increasing vector of timestamps (in days)
  vector<double> holder(N, 0); // use 1 indexed version
  int len = X.length();
  NumericVector out(len);

  // if (len != days.length())
  //   Rf_error("length of 'X' and 'days' must aggree");

  double edelta = exp( - 1/theta );
  
  if (len > 0 && N > 0) {
    holder[X[0] - 1] = 1.0;
    out[0] = 1.0;

    for (int i = 1; i < len; i++) {
      double sum = 0.0;
      // this counfounds duration and diversity, so use edelta exp( - alpha ) instead
      // double edelta = exp( -alpha * (days[i] - days[i - 1]) );
      int xi = X[i] - 1;
      if (xi < 0 || xi >= N )
        Rf_error("X must hold indexes from 1 to %d (N)", N);

      for (int j = 0; j < N; j++) {
        if (j != xi) {
          // everything else except current object decays
          holder[j] = holder[j] * edelta;
          sum = sum + holder[j];
        }
      }
      // current one is reset to 1
      holder[xi] = 1.0;
      out[i] = sum + 1.0;
    }
  }
  return out;
}

//' @export
// [[Rcpp::export]]
IntegerVector c_cum_unique_count(IntegerVector& X, int N){
  // Count cumulatively number of different elements in X 
  // N is a total number of distinct elements in X
  // X contains integer numbers in [1, N]

  vector<int> holder(N, 0);
  int len = X.length(), acc = 1;
  IntegerVector out(len);

  if (len > 0 && N > 0){

    out[0] = acc;

    for (int i = 1; i < len; i++) {

      int xi = X[i] - 1;
      if (xi < 0 || xi >=  N )
        Rf_error("X must hold indexes from 1 to %d (accessing %d at i=%d)", N, X[i], i);
      
      if( holder[X[i] - 1] < 1 ){
        holder[X[i] - 1] = 1;
        out[i] = ++acc;
      } else {
        out[i] = acc;
      }
    }
    
  }
  
  return out;
}
