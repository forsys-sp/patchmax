#ifndef GRAPH_H
#define GRAPH_H

#include <queue>
#include <string>
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace std;

// Priority queue comparator
struct comp{
  bool operator()(const pair<int, double> &a, const pair<int, double> &b){
    return a.second > b.second;
  }
};

// Data structures
using G = vector<vector<pair<int, double> > >;
using PQ = priority_queue<pair<int, double>, vector<pair<int, double> >, comp >;
using IVec = vector<int>;
using DVec = vector<double>;
using SVec = vector<string>;
using VVec = vector<SVec >;
using VIVec = vector<IVec >;

// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp11)]]

// Graph object
class Graph{
public:
  
  // Mandatory attribute
  int nbnode;
  int nbedge;
  int n_core;
  G data;
  
  // Parallel graph : 3 vectors (adjacency list)
  IVec nodeG;
  IVec indG;
  DVec wG;
  DVec add; // additional weight
  
  // Constructor
  Graph(IVec &gfrom, IVec &gto, DVec &gw, int nb);
  
  // Constructor with additional weight
  Graph(IVec &gfrom, IVec &gto, DVec &gw, DVec &gadd, int nb);
  
  // Setters
  void to_adj_list(bool reversed);
  
  // return edge description from current graph
  Rcpp::List getEdges();
  
  Rcpp::NumericMatrix routing_dmat(IVec dep, IVec arr, double max_aux);
};



#endif
