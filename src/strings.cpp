// this doesn't work at all, omp has no effect or is not even triggered

#include <Rcpp.h>
#include <omp.h>
// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;

// [[Rcpp::export]]
std::vector< std::string > c_fast_paste0(std::vector<std::string>& str1, std::vector<std::string>& str2) {
  
  size_t len1 = str1.size();
  size_t len2 = str2.size();

  if(len1 != len1 && !(len1 == 1 || len2 == 1)){
    throw std::invalid_argument("str1 and str2 must have same legnth or be of length 1.");
  }
  omp_set_num_threads(8);
  
  std::vector<std::string> out(std::max(len1, len2));
  
  if (len1 == 1){
    std::string str = str1[0];
#pragma omp parallel for
    for (size_t i = 0; i < len2; i++){
      out[i] = str + str2[i];
    }
    return out;
  }

  if (len2 == 1) {
    std::string str = str2[0];
#pragma omp parallel for
    for (size_t i = 0; i < len1; i++) {
      out[i] = str1[i] + str;
    }
    return out;
  }

#pragma omp parallel for
  for(size_t i=0; i < len1; i++ ) {
    out[i] = str1[i] + str2[i];
  }

  return out;
}
