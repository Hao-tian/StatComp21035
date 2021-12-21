

#include <Rcpp.h>
using namespace Rcpp;

//' @title gibbs_cpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param thin the number of between-sample random numbers
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbs_cpp(5,2,3) 
//'  par(mfrow=c(2,1));
//'  plot(rnC[,1],type='l') 
//'  plot(rnC[,2],type='l')
//' } 
//' @export

// [[Rcpp::export]]
NumericMatrix gibbs_cpp(int n, int a, int b){
  NumericMatrix df(10000,2);
  NumericVector X(10000);
  NumericVector Y(10000);
  NumericVector temp(1);
  
  X[0]=1;
  Y[0]=1;
  
  for (int i=1;i<10000;i++){
    temp=rbeta(1,X[i-1]+a,n-X[i-1]+b);
    Y[i]=temp[0];
    temp=rbinom(1,n,Y[i]);
    X[i]=temp[0];
  }
  
  df(_,0)=X;
  df(_,1)=Y;
  
  return df;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

