#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
double c_apk1(const int k, const int actual , const IntegerVector& predicted) {
  double score = 0.0;
  int cnt = 0;
  int N = std::min(k, (int) predicted.size());

  for (int i = 0; i < N; i++ ){
    if (predicted[i] == actual){
      cnt++;
      score += ((double) cnt)/(i + 1);
    }
  }
  return score;
}


