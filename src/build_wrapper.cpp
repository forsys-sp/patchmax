#include "graph.h"
#include <string>
#include <vector>
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace std;

void check_nas_mat(Rcpp::NumericMatrix &mat){
  for (unsigned int i = 0; i < mat.size(); i++){
    if (mat[i] == numeric_limits<double>::max()) mat[i] = Rcpp::NumericVector::get_na();
  }
}

// [[Rcpp::export]]
Rcpp::NumericMatrix cppdistmat(std::vector<int> &gfrom, std::vector<int> &gto, std::vector<double> &gw, int nb,
                               std::vector<int> dep, std::vector<int> arr, double &max_aux){
  Graph network(gfrom, gto, gw, nb);
  network.to_adj_list(false);
  Rcpp::NumericMatrix result = Rcpp::wrap(network.routing_dmat(dep, arr, max_aux));
  check_nas_mat(result);
  return result;
}

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
Rcpp::NumericMatrix cpppadd(std::vector<int> &gfrom, std::vector<int> &gto, std::vector<double> &gw, std::vector<double> &gadd, int nb,
            std::vector<int> dep, std::vector<int> arr, double &max_aux){
  Graph network(gfrom, gto, gw, gadd, nb);
  Rcpp::NumericMatrix result = Rcpp::wrap(network.routing_dmat(dep, arr, max_aux));
  check_nas_mat(result);
  return result;
}
