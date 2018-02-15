#include <iostream>
#include <Rcpp.h>
#include <limits>
#include <vector>
#include <forward_list>
#include <functional>

// Enable C++11 via plugin
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace std::placeholders;

// roll FUN on window starting at current-left_bound and ending at
// current_date-right_bound. 
NumericVector c_roll(std::function<double (int si, int ei, const NumericVector& X)> fun,
                     NumericVector& DATE,
                     NumericVector& X,
                     double left_bound,
                     double right_bound,
                     bool left_open,
                     bool right_open){
  if(DATE.length() != X.length()){
	Rf_error("Length of 'IX' and 'X' vectors differ.");
  }
  if(right_bound < left_bound){
    std::swap(right_bound, left_bound);
  }
  
  int N = DATE.length();
  std::forward_list<double> out_vals, out_dates; 

  // forward, start and end indexes
  int fi = 0; // forward index
  int si = 0; // start index, ix of the first element in the window queue
  int ei = 0; // end index, ix of the next element of the last element in the window

  double val;
  
  while(fi < N) {
    
	double fd = DATE[fi];         // forward date
	double sd = fd + left_bound;  // start date (of the window)
	double ed = fd + right_bound; // end date (of the window)

	// out_dates.push_front(fd);

	// move fi to next date;
	fi++;
    int nsame = 1;
	while(fi < N && fd == DATE[fi]){
	  fi++;
      nsame++;
	}

	// Make interval sd <= date <= ed
	while(si < N & DATE[si] < sd){
	  si++;
	}
	while(ei < (N - 1) & DATE[ei] < ed){
	  ei++;
	}

    int tsi = si + left_open, tei = ei - right_open;

    
	if(tsi > tei){
	  val = NA_REAL;
	} else {
	  /* std::cout << "fd: " << fd << " tsi: " << tsi << " tei: " << tei << " X[tei]:" << X[tei] << std::endl; */
      val = fun(tsi, tei, X);
	}
    while (nsame > 0){
      nsame--;
      out_vals.push_front(val);
    }
  }

  // out_dates.reverse();
  out_vals.reverse();
  
  // NumericVector ix = NumericVector(out_dates.begin(), out_dates.end());
  // NumericVector vals = NumericVector(out_vals.begin(), out_vals.end());
  
  // return DataFrame::create(Named("ix") = out_dates, 
  //   					   Named("val") = out_vals);
  return NumericVector(out_vals.begin(), out_vals.end());
}

// receive start index (si), end index + 1 (ei) an the original vector X
inline double vec_min(int si, int ei, const NumericVector& X) {
  double accum = R_PosInf;
  for(int i = si; i <= ei; i++){
	accum = std::min(accum, X[i]);
  }
  if (accum == R_PosInf) return NA_REAL;
  else return accum;
}

inline double vec_max(int si, int ei, const NumericVector& X) {
  double accum = R_NegInf;
  for(int i = si; i <= ei; i++){
    accum = std::max(accum, X[i]);
  }
  if (accum == R_NegInf) return NA_REAL;
  else return accum;
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_min(SEXP DATE, NumericVector& X,
                         double left_bound, double right_bound,
                         bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll(vec_min, tt, X, left_bound, right_bound, left_open, right_open);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_max(SEXP DATE, NumericVector& X,
                         double left_bound, double right_bound,
                         bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll(vec_max, tt, X, left_bound, right_bound, left_open, right_open);
}

double vec_mean(int si, int ei, const NumericVector& X) {
  double accum = 0;
  size_t nr = 0;
  for(int i = si; i <= ei; i++){
    // this NA check doesn't work on integers
    if (!ISNA(X[i])){
      accum += X[i];
      nr++;
    }
  }
  if (nr) return accum/nr;
  else return NA_REAL;
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_mean(SEXP DATE, NumericVector& X, 
                          double left_bound, double right_bound,
                          bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll(vec_mean, tt, X, left_bound, right_bound, left_open, right_open);
}

double vec_sum(int si, int ei, const NumericVector& X) {
  double accum = 0;
  for(int i = si; i <= ei; i++){
    // this NA check doesn't work on integers
    if (!ISNA(X[i])) {
      accum += X[i];
    }
  }
  return accum;
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_sum(SEXP DATE, NumericVector& X, 
                         double left_bound, double right_bound,
                         bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll(vec_sum, tt, X, left_bound, right_bound, left_open, right_open);
}
 
double vec_prod(int si, int ei, const NumericVector& X) {
  double accum = 0;
  for(int i = si; i <= ei; i++){
    if (!ISNA(X[i])) {
      accum *= X[i];
    }
  }
  return accum;
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_prod(SEXP DATE, NumericVector& X, 
                          double left_bound, double right_bound,
                          bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll(vec_prod, tt, X, left_bound, right_bound, left_open, right_open);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_first(SEXP DATE, NumericVector& X, 
                           double left_bound, double right_bound,
                           bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll([](int si,  int ei, const NumericVector& X) { return X[si]; },
				tt, X, left_bound, right_bound, left_open, right_open);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_last(SEXP DATE, NumericVector& X, 
                          double left_bound, double right_bound,
                          bool left_open, bool right_open){
  NumericVector tt(DATE);
  return c_roll([](int si, int ei, const NumericVector& X) { return X[ei]; },
				tt, X, left_bound, right_bound, left_open, right_open);
}


// QUANTILES
double quantile(const std::vector<double>& z, double prob) {
  int n = z.size();
  if(n == 1){
	return z[0];
  } else {
	int lo = floor((n - 1) * prob);
	int hi = lo + 1;
	return z[lo]*prob + z[hi]*(1 - prob);
  } 
}

double vec_quantile(int si,  int ei, const NumericVector& X, double prob){
  // quantile setup
  printf("se: %d %d\n", si, ei);
  NumericVector::const_iterator first = X.begin() + si;
  NumericVector::const_iterator last = X.begin() + (ei + 1);
  std::vector<double> z(first, last);
  printf("len: %d\n", (int) z.size());
  printf("sevals: %f %f\n", z.front(), z.back());
  sort(z.begin(), z.end());    	
  return quantile(z, prob);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_quantile(SEXP DATE, NumericVector& X, 
                              double left_bound, double right_bound,
                              bool left_open, bool right_open,
                              double prob){
  NumericVector tt(DATE);
  return c_roll(std::bind(vec_quantile, _1, _2, _3, prob),
				tt, X, left_bound, right_bound, left_open, right_open);
}


// UTILS

// like base cummin but ignores NAs
//' @export
// [[Rcpp::export]]
NumericVector c_cummin(const NumericVector& X){
  NumericVector out(X.length());
  if (X.length()){
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < X.length(); i++){
      if (!ISNA(X[i]) && X[i] != NA_INTEGER){
        accum = std::min(X[i], accum);
      }
      out[i] = accum;
    }
    return out;
  }
}

// like base cummax but ignores NAs
//' @export
// [[Rcpp::export]]
NumericVector c_cummax(const NumericVector& X){
  NumericVector out(X.length());
  if (X.length()){
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < X.length(); i++){
      if (!ISNA(X[i]) && X[i] != NA_INTEGER){
        accum = std::max(X[i], accum);
      }
      out[i] = accum;
    }
    return out;
  }
}
