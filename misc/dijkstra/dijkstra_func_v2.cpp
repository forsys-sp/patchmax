#include <Rcpp.h>
#include <queue>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector dijkstra_cpp(DataFrame edgelist, NumericVector nodelist, int start) {
  
  
  // the data fame into C++ vectors
  std::vector<double> e_f = 
    Rcpp::as< std::vector<double> >(edgelist["from"]);
  std::vector<int> e_t = 
    Rcpp::as< std::vector<int> >(edgelist["to"]);
  std::vector<int> e_d = 
    Rcpp::as< std::vector<int> >(edgelist["dist"]);
  
  // extract edge list vector data 
  // NumericVector e_f = edgelist["from"];
  // NumericVector e_t = edgelist["to"];
  // NumericVector e_d = edgelist["dist"];
  
  // create list of unvisited nodes
  NumericVector unvisited = unique(nodelist);
  NumericVector neigbors;
  
  NumericVector distances = rep(R_PosInf, unvisited.size());
  distances[start] = 0;
  
  using PQ = priority_queue<pair<int, double>, vector<pair<int, double>>>;
  PQ Q;
  Q.push(make_pair(start, 0.0));
  
  int v = Q.top().first;
  //double w = Q.top().second;
  //Rprintf("%i %d", v, w);
  
  Q.pop();
  
  //neigbors = e_t[e_f == v];
  //print(neigbors);
  //for(int j = 0; j < neigbors.length(); j++){
  //  if(e_d[e_f == v & e_t == neigbors[j]]);
  //  NumericVector w2 = e_d[e_f == v & e_t == neigbors[j]];
  //}
    
  return distances;
}

/*** R
edgelist_test <- data.frame(
  from=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
  to=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
  dist=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9),
  aux=rep(10,18))
dijkstra_cpp(edgelist_test, 1)
*/