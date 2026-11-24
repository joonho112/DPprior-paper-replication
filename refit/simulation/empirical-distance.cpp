// Author: JoonHo Lee (jlee296@ua.edu)
#include <Rcpp.h>
#include <algorithm>
#include <cmath>

// Both arguments must be sorted ascending. Visit each distinct jump only once.
// [[Rcpp::export]]
Rcpp::NumericVector sorted_cdf_distance(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  const R_xlen_t n = x.size(), m = y.size();
  if (!n || !m) Rcpp::stop("Empty distribution");
  R_xlen_t i = 0, j = 0;
  double last = std::min(x[0], y[0]), gap = 0.0, ks = 0.0;
  long double integral = 0.0;
  while (i < n || j < m) {
    const double next = i == n ? y[j] : j == m ? x[i] : std::min(x[i], y[j]);
    integral += (next - last) * gap * gap;
    while (i < n && x[i] == next) ++i;
    while (j < m && y[j] == next) ++j;
    gap = static_cast<double>(i) / n - static_cast<double>(j) / m;
    ks = std::max(ks, std::abs(gap));
    last = next;
  }
  return Rcpp::NumericVector::create(Rcpp::Named("ISEL") = static_cast<double>(integral),
                                     Rcpp::Named("KS") = ks);
}
