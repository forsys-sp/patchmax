#ifndef DISTANCE_MAT_H
#define DISTANCE_MAT_H

#include <vector>
#include <RcppParallel.h>
#include <Rcpp.h>
#include "graph.h"
using namespace RcppParallel;


struct distanceMat : public Worker{
  
  // graph pointer to avoid copying input
  const Graph *m_gr;
  IVec m_dep;
  IVec m_arr;
  double d_max_aux;
  bool add;
  
  RMatrix<double> m_result;
  
  // constructor
  distanceMat(Graph *gr, IVec dep, IVec arr, double max_aux, Rcpp::NumericMatrix result);
  
  // algorithms as member functions
  void dijkstra_mat(std::size_t begin, std::size_t end);
  
  // overload operator ()
  void operator()(std::size_t begin, std::size_t end);
  
};

#endif
