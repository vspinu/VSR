// Copyright: 2011 by Andreas Eckner
// License: Copying and distribution of this file, with or without modification,
//          are permitted in any medium without royalty provided the copyright
//          notice and this notice are preserved.

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "operators.h"


// Declare function prototypes
static void print_ts(double values[], double times[], int n);


// Print time series values and observation times
void print_ts(double values[], double times[], int n)
{
  // Print header
  int i;
  printf("--------------\n");
  printf("Time     Value\n");
  printf("--------------\n");

  // Print time series values and times
  for (i=0; i < n; i++)
    printf("%.2f   %7.3f\n", times[i], values[i]); 
}


// Demo of functionality
int main()
{
  // Define demo-specific constants
  int n = 6, obs_refresh = 3;
  double tau_sma = 2.5, tau_ema = 1.25, tau_long = 1e10;
  double m1 = 1, m2 = 1.5;
  
  // Allocate memory
  int i;
  double *values, *times, *out;
  values = malloc(n * sizeof(double));
  times = malloc(n * sizeof(double));
  out = malloc(n * sizeof(double));
  assert(values != NULL);
  assert(times != NULL);
  assert(out != NULL);
  
  // Generate sample time series
  for (i=0; i < n; i++)
  {
    times[i] = i + cos(i);
    values[i] = (double) i / (n-1);
  }
  printf("Input time series X:\n");
  print_ts(values, times, n);

  // rolling_obs
  rolling_obs(values, times, &n, out, &tau_sma);
  printf("\nrolling_obs(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);

  // rolling_sum
  rolling_sum(values, times, &n, out, &tau_sma);
  printf("\nrolling_sum(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);



  /* Simple moving average analog to discrete time */
  printf("\n*** Simple moving average analog to discrete time ***\n");

  // SMA_eq
  sma_eq(values, times, &n, out, &tau_sma);
  printf("\nSMA_eq(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);
  
  // SMA_eq_stable
  sma_eq_stable(values, times, &n, out, &tau_sma, &obs_refresh);
  printf("\nSMA_eq_stable(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);

  // rolling_moment_eq
  rolling_moment_eq(values, times, &n, out, &tau_sma, &m1);
  printf("\nrolling_moment_eq(X, %.2f, %.2f):\n", tau_sma, m1);
  print_ts(out, times, n);



  /* Simple moving average with last-point sampling */
  printf("\n*** Simple moving average with last-point sampling ***\n");
  
  // SMA
  sma_last(values, times, &n, out, &tau_sma);
  printf("\nSMA_last(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);

  // rolling_moment
  rolling_moment(values, times, &n, out, &tau_sma, &m1);
  printf("\nrolling_moment(X, %.2f, %.2f):\n", tau_sma, m1);
  print_ts(out, times, n);
  


  /* Simple moving average with linear interpolation */
  printf("\n*** Simple moving average with linear interpolation ***\n");

  // SMA_lin
  sma_lin(values, times, &n, out, &tau_sma);
  printf("\nSMA_lin(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);
  
  // rolling_moment_lin
  rolling_moment_lin(values, times, &n, out, &tau_sma, &m1);
  printf("\nrolling_moment_lin(X, %.2f, %.2f):\n", tau_sma, m1);
  print_ts(out, times, n);



  /* Exponential moving averages */
  printf("\n*** Exponential moving averages ***\n");
  
  // ema_eq
  ema_eq(values, times, &n, out, &tau_ema, &values[0]);
  printf("\nEMA_eq(X, %.2f):\n", tau_ema);
  print_ts(out, times, n);

  // ema
  ema_last(values, times, &n, out, &tau_ema, &values[0]);
  printf("\nEMA_last(X, %.2f):\n", tau_ema);
  print_ts(out, times, n);

  // ema_lin
  ema_lin(values, times, &n, out, &tau_ema, &values[0]);
  printf("\nEMA_lin(X, %.2f):\n", tau_ema);
  print_ts(out, times, n);

  // ema_lin
  printf("\nEMA with very long time window => output should be (nearly) constant:");
  ema_lin(values, times, &n, out, &tau_long, &values[0]);
  printf("\nEMA_lin(X, %.2f):\n", tau_long);
  print_ts(out, times, n);


  /* Rolling moments */
  printf("\n*** Rolling moments ***\n");

  // rolling_moment_eq - part2
  rolling_moment_eq(values, times, &n, out, &tau_sma, &m2);
  printf("\nrolling_moment_eq(X, %.2f, %.2f):\n", tau_sma, m2);
  print_ts(out, times, n);

  // rolling_moment - part2
  rolling_moment(values, times, &n, out, &tau_sma, &m2);
  printf("\nrolling_moment(X, %.2f, %.2f):\n", tau_sma, m2);
  print_ts(out, times, n);
  
  // rolling_moment_lin - part2
  rolling_moment_lin(values, times, &n, out, &tau_sma, &m2);
  printf("\nrolling_moment_lin(X, %.2f, %.2f):\n", tau_sma, m2);
  print_ts(out, times, n);



  /* Rolling median and quantiles */
  printf("\n*** Rolling median and quantiles ***\n"); 
 
  // rolling_median
  rolling_median(values, times, &n, out, &tau_sma);
  printf("\nrolling_median(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);
  
  // rolling_quantile
  rolling_quantile_eq(values, times, &n, out, &tau_sma);
  printf("\nrolling_quantile_eq(X, %.2f):\n", tau_sma);
  print_ts(out, times, n);

  // Free memory
  printf("\nPress <ENTER> to exit the program.\n");
  getchar();
  free(values);
  free(times);
  free(out);
  return 0;
}
