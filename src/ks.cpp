#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <vector>

using Rcpp::NumericVector;

namespace {

void matrixMultiply(const std::vector<double>& a,
                    const std::vector<double>& b,
                    std::vector<double>& result,
                    int size)
{
  for(int i = 0; i < size; ++i) {
    for(int j = 0; j < size; ++j) {
      double sum = 0.0;

      for(int k = 0; k < size; ++k) {
        sum += a[i * size + k] * b[k * size + j];
      }

      result[i * size + j] = sum;
    }
  }
}

void matrixPower(const std::vector<double>& matrix,
                 int matrixExponent,
                 std::vector<double>& result,
                 int& resultExponent,
                 int size,
                 int power)
{
  if(power == 1) {
    result = matrix;
    resultExponent = matrixExponent;
    return;
  }

  std::vector<double> squared(size * size);

  matrixPower(matrix, matrixExponent, result, resultExponent,
              size, power / 2);
  matrixMultiply(result, result, squared, size);

  int squaredExponent = 2 * resultExponent;

  if(power % 2 == 0) {
    result = squared;
    resultExponent = squaredExponent;
  } else {
    matrixMultiply(matrix, squared, result, size);
    resultExponent = matrixExponent + squaredExponent;
  }

  if(result[(size / 2) * size + size / 2] > 1e140) {
    for(double& value : result) {
      value *= 1e-140;
    }
    resultExponent += 140;
  }
}

} // namespace


// [[Rcpp::export]]
double pKolmogorov2x_cpp(double statistic, int n)
{
  int k = static_cast<int>(n * statistic) + 1;
  int size = 2 * k - 1;
  double h = k - n * statistic;

  std::vector<double> hMatrix(size * size, 0.0);
  std::vector<double> qMatrix(size * size, 0.0);

  for(int i = 0; i < size; ++i) {
    for(int j = 0; j < size; ++j) {
      if(i - j + 1 >= 0) {
        hMatrix[i * size + j] = 1.0;
      }
    }
  }

  for(int i = 0; i < size; ++i) {
    hMatrix[i * size] -= std::pow(h, i + 1);
    hMatrix[(size - 1) * size + i] -= std::pow(h, size - i);
  }

  if(2 * h - 1 > 0) {
    hMatrix[(size - 1) * size] += std::pow(2 * h - 1, size);
  }

  for(int i = 0; i < size; ++i) {
    for(int j = 0; j < size; ++j) {
      if(i - j + 1 > 0) {
        for(int divisor = 1; divisor <= i - j + 1; ++divisor) {
          hMatrix[i * size + j] /= divisor;
        }
      }
    }
  }

  int hExponent = 0;
  int qExponent = 0;
  matrixPower(hMatrix, hExponent, qMatrix, qExponent, size, n);

  double probability = qMatrix[(k - 1) * size + k - 1];

  for(int i = 1; i <= n; ++i) {
    probability *= static_cast<double>(i) / n;

    if(probability < 1e-140) {
      probability *= 1e140;
      qExponent -= 140;
    }
  }

  return probability * std::pow(10.0, qExponent);
}


// [[Rcpp::export]]
double pSmirnov2x_cpp(double statistic, int nx, int ny)
{
  if(nx > ny) {
    std::swap(nx, ny);
  }

  double nxDouble = static_cast<double>(nx);
  double nyDouble = static_cast<double>(ny);
  double q = (0.5 + std::floor(statistic * nxDouble * nyDouble - 1e-7)) /
             (nxDouble * nyDouble);

  std::vector<double> probability(ny + 1);

  for(int j = 0; j <= ny; ++j) {
    probability[j] = (static_cast<double>(j) / nyDouble > q) ? 0.0 : 1.0;
  }

  for(int i = 1; i <= nx; ++i) {
    double weight = static_cast<double>(i) / (i + ny);
    probability[0] = (static_cast<double>(i) / nxDouble > q) ?
      0.0 : weight * probability[0];

    for(int j = 1; j <= ny; ++j) {
      if(std::fabs(static_cast<double>(i) / nxDouble -
                   static_cast<double>(j) / nyDouble) > q) {
        probability[j] = 0.0;
      } else {
        probability[j] = weight * probability[j] + probability[j - 1];
      }
    }
  }

  return probability[ny];
}


// [[Rcpp::export]]
NumericVector pKS2_cpp(NumericVector statistic, double tol)
{
  NumericVector probability = Rcpp::clone(statistic);
  int n = probability.size();
  int maxTerms = static_cast<int>(std::sqrt(2.0 - std::log(tol)));

  for(int i = 0; i < n; ++i) {
    if(probability[i] < 1.0) {
      double z = -(M_PI_2 * M_PI_4) /
                 (probability[i] * probability[i]);
      double logProbability = std::log(probability[i]);
      double sum = 0.0;

      for(int k = 1; k < maxTerms; k += 2) {
        sum += std::exp(k * k * z - logProbability);
      }

      probability[i] = sum / M_1_SQRT_2PI;
    } else {
      double z = -2.0 * probability[i] * probability[i];
      double sign = -1.0;
      double previous = 0.0;
      double current = 1.0;
      int k = 1;

      while(std::fabs(previous - current) > tol) {
        previous = current;
        current += 2.0 * sign * std::exp(z * k * k);
        sign *= -1.0;
        ++k;
      }

      probability[i] = current;
    }
  }

  return probability;
}
