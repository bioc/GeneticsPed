/* sargolzaei.c
 *-------------------------------------------------------------------------
 * What: Compute inbreeding coefficients Sargolzaei:2005
 * $Id: sargolzaei.c 1149 2007-03-01 11:24:18Z ggorjan $
 * Time-stamp: <2007-02-28 23:59:37 ggorjan>
 *-----------------------------------------------------------------------*/

void MyQuickSort( int Ped[][2] , int *SId );
void sargolzaei( int* n , int * m );

void sargolzaei( int* n , int * m )
{
  // n; number of subjects in total
  // m; number of fathers and mothers in total

  /* Variable declaration  */

  // integer vars
  int i, j, k, rN, rS, S, D, MIP, N;
  // main and reduced pedigrees
  int Ped[*n + 1][2], rPed[*m + 1][2];
  // sorted animals id based on the id of their fathers
  int SId[*n + 1];
  // new id of ancestors at position of their original id
  int Link[*n + 1];
  // maximum new id of parents for each paternal group at position of the
  // new id of each father
  int MaxIdP[*m + 1];
  // inbreeding coefficients, within family segregation vars and x arrays
  float F[*n + 1], B[*m + 1], x[*m + 1];

  /* Kernel */

  F[0] = -1.0f, x[0] = 0.0f, Link[0] = 0; // set values for the unknown parent
  for(rN = i = 1; i <= *n; rN++, i++) {   // extract and recode ancestors
    SId[i] = i, Link[i] = 0; if(i <= *m) x[i] = 0.0f; // initialization
    S = Ped[i][0], D = Ped[i][1];
    if(S && !Link[S]) {
      MaxIdP[rN] = Link[S] = rN;
      rPed[rN][0] = Link[Ped[S][0]];
      rPed[rN++][1] = Link[Ped[D][1]];
    }
    if(D && !Link[D]) {
      Link[D] = rN;
      rPed[rN][0] = Link[Ped[D][0]];
      rPed[rN++][1] = Link[Ped[D][1]];
    }
    if(MaxIdP[Link[S]] < Link[D]) MaxIdP[Link[S]] = Link[D];
    // determine maximum id of parents for each paternal group
  }

  // sort subjects according to id of their sires into SId
  MyQuickSort(Ped, SId);

  for(k = i = 1; i <= *n;) {
    if(!Ped[SId[i]][0]) F[SId[i++]] = 0.0f; // father is unknown
    else {
      S = Ped[SId[i]][0], rS = Link[S], MIP = MaxIdP[rS];
      x[rS] = 1.0f;
      for(; k <= S; ++k)    // compute within family segregation variances
        if(Link[k]) B[Link[k]] = 0.5f - 0.25f * (F[Ped[k][0]] + F[Ped[k][1]]);
      for(j = rS; j; -j) {  // trace back to reduced pedigree
        if(x[j]) {          // consider only ancestors of a father
          if(rPed[j][0]) x[rPed[j][0]] += x[j] * 0.5f;
          if(rPed[j][1]) x[rPed[j][1]] += x[j] * 0.5f;
          x[j] = B[j];
        }
      }
    }
    for(j = 1; j < MIP; j++) // trace forth the reduced pedigree
      x[j] += (x[rPed[j][0]] + x[rPed[j][1]]) * 0.5f;
    for(i=0; i < N; i++)     // obtain F for progeny of the current father
      if(S != Ped[SId[i]][0]) break;
      else F[SId[i]] = x[Link[Ped[SId[i]][1]]] * 0.5f;
    for(j = 1; j <= MIP; j++) x[j] = 0.0f; // set to 0 for next evaluation
  }                                        // of father

}

void MyQuickSort( int Ped[][2] , int *SId )
{
  return;
}

/*-------------------------------------------------------------------------
 * sargolzaei.c ends here */
