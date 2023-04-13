#include "graph.h"
#include "distance_mat.h"
#include <string>
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace std;

// Constructor
Graph::Graph(vector<int> &gfrom, vector<int> &gto, vector<double> &gw, int nb){

  nbedge = gfrom.size();
  nbnode = nb;

  data = G(nbnode);
  add.resize(0);

  for (int i = 0; i != nbedge; ++i) {
    data[gfrom[i]].push_back(std::make_pair(gto[i], gw[i]));
  }
}

// # Constructor with additional weight
Graph::Graph(IVec &gfrom, IVec &gto, DVec &gw, DVec &gadd, int nb){

  nbedge = gfrom.size();
  nbnode = nb;
  
  data = G(nbnode); 
  vector<IVec> data2(nbnode); // forward

  for (int i = 0; i != nbedge; ++i) {
    data[gfrom[i]].push_back(std::make_pair(gto[i], gw[i]));
    data2[gfrom[i]].push_back(i);
  }

  // adj list
  int count = 0;
  for (int i = 0; i < nbnode; i++){
    count += data[i].size();
  }

  // forward
  nodeG.resize(count);
  wG.resize(count);
  add.resize(count);
  indG.resize(nbnode + 1);

  count=0;
  for (int i=0; i < data2.size();i++){
    indG[i] = count;
    for (int j = 0; j < data2[i].size(); j++){
      nodeG[count] = gto[data2[i][j]];
      wG[count] = gw[data2[i][j]];
      add[count] = gadd[data2[i][j]];
      count += 1;
    }
  }
  indG[nbnode]=count;
}

// Setters
Rcpp::List Graph::getEdges(){
  Rcpp::List edges(3);

  nbedge = 0;
  for (int i=0; i < data.size(); i++){
    nbedge += data[i].size();
  }

  IVec Newfrom(nbedge);
  IVec Newto(nbedge);
  DVec Neww(nbedge);

  int index=0;
  for (int i=0; i < data.size();i++){
    for (int j=0; j < data[i].size();j++){

      Newfrom[index]=i;
      Newto[index]= data[i][j].first;
      Neww[index]= data[i][j].second;
      index+=1;

    }
  }

  edges[0] = Newfrom;
  edges[1] = Newto;
  edges[2] = Neww;

  return edges;
}

void Graph::to_adj_list(bool reversed){
  int count = 0;

  for (int i = 0; i < nbnode; i++){
    count += data[i].size();
  }

  nodeG.resize(count);
  wG.resize(count);
  indG.resize(nbnode + 1);
  count=0;

  for (int i=0; i < data.size();i++){
    indG[i] = count;

    for (int j = 0; j < data[i].size(); j++){
      nodeG[count] = data[i][j].first;
      wG[count] = data[i][j].second;
      count += 1;
    }
  }
  indG[nbnode]=count;
}

// dijkstra's calculation of distances
Rcpp::NumericMatrix Graph::routing_dmat(IVec dep, IVec arr, double max_aux){
  Rcpp::NumericMatrix result(dep.size(), arr.size());
  distanceMat dijfunc(this, dep, arr, max_aux, result);
  parallelFor(0, dep.size(), dijfunc, 1, 12);
  return result;
}
