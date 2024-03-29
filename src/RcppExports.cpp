// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cppdistmat
Rcpp::NumericMatrix cppdistmat(std::vector<int>& gfrom, std::vector<int>& gto, std::vector<double>& gw, int nb, std::vector<int> dep, std::vector<int> arr, double& max_aux);
RcppExport SEXP _patchmax_cppdistmat(SEXP gfromSEXP, SEXP gtoSEXP, SEXP gwSEXP, SEXP nbSEXP, SEXP depSEXP, SEXP arrSEXP, SEXP max_auxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int>& >::type gfrom(gfromSEXP);
    Rcpp::traits::input_parameter< std::vector<int>& >::type gto(gtoSEXP);
    Rcpp::traits::input_parameter< std::vector<double>& >::type gw(gwSEXP);
    Rcpp::traits::input_parameter< int >::type nb(nbSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type dep(depSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< double& >::type max_aux(max_auxSEXP);
    rcpp_result_gen = Rcpp::wrap(cppdistmat(gfrom, gto, gw, nb, dep, arr, max_aux));
    return rcpp_result_gen;
END_RCPP
}
// cpppadd
Rcpp::NumericMatrix cpppadd(std::vector<int>& gfrom, std::vector<int>& gto, std::vector<double>& gw, std::vector<double>& gadd, int nb, std::vector<int> dep, std::vector<int> arr, double& max_aux);
RcppExport SEXP _patchmax_cpppadd(SEXP gfromSEXP, SEXP gtoSEXP, SEXP gwSEXP, SEXP gaddSEXP, SEXP nbSEXP, SEXP depSEXP, SEXP arrSEXP, SEXP max_auxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int>& >::type gfrom(gfromSEXP);
    Rcpp::traits::input_parameter< std::vector<int>& >::type gto(gtoSEXP);
    Rcpp::traits::input_parameter< std::vector<double>& >::type gw(gwSEXP);
    Rcpp::traits::input_parameter< std::vector<double>& >::type gadd(gaddSEXP);
    Rcpp::traits::input_parameter< int >::type nb(nbSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type dep(depSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< double& >::type max_aux(max_auxSEXP);
    rcpp_result_gen = Rcpp::wrap(cpppadd(gfrom, gto, gw, gadd, nb, dep, arr, max_aux));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_patchmax_cppdistmat", (DL_FUNC) &_patchmax_cppdistmat, 7},
    {"_patchmax_cpppadd", (DL_FUNC) &_patchmax_cpppadd, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_patchmax(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
