/* sargolzaei.c
 *-------------------------------------------------------------------------
 * What: Compute inbreeding coefficients Sargolzaei:2005
 * $Id: sargolzaei.c 1149 2007-03-01 11:24:18Z ggorjan $
 * Time-stamp: <2007-09-20 12:50:12 ggorjan>
 *-----------------------------------------------------------------------*/

void MyQuickSort(int Ped[][2], int *SId);
void sargolzaei(int *n, int *m);

void sargolzaei(int *n, int *m)
{
  // n; number of individuals in total
  // m; number of fathers and mothers in total

  /* Variable declaration  */

  // integer vars
  int i=0, j=0, k=0, rN=0, rS=0, S=0, D=0, MIP=0;
  // main and reduced pedigrees
  int Ped[*n + 1][2], rPed[*m + 1][2];
  // sorted individuals id based on the id of their fathers
  int SId[*n + 1];
  // new id of ancestors at position of their original id
  int Link[*n + 1];
  // maximum new id of parents for each paternal group at position of the
  // new id of each father
  int MaxIdP[*m + 1];
  // inbreeding coefficients, within family segregation vars and x arrays
  float F[*n + 1], B[*m + 1], x[*m + 1];

  /* Kernel */

  // set values for the unknown parent
  F[0] = -1.0f;
  x[0] = 0.0f;
  Link[0] = 0;

  // extract and recode ancestors
  for(rN = i = 1; i <= *n; rN++, i++) {

    // initialization
    SId[i] = i;
    Link[i] = 0;
    if(i <= *m)
      x[i] = 0.0f;
    S = Ped[i][0];
    D = Ped[i][1];

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

    // determine maximum id of parents for each paternal group
    if(MaxIdP[Link[S]] < Link[D])
      MaxIdP[Link[S]] = Link[D];
  }

  // sort individuals according to id of their fathers into SId
  MyQuickSort(Ped, SId);

  for(k = i = 1; i <= *n;) {

    if(!Ped[SId[i]][0])
      F[SId[i++]] = 0.0f;   // father is unknown
    else {
      S = Ped[SId[i]][0];
      rS = Link[S];
      MIP = MaxIdP[rS];
      x[rS] = 1.0f;

      for(; k <= S; ++k)    // compute within family segregation variances
        if(Link[k]) B[Link[k]] = 0.5f - 0.25f * (F[Ped[k][0]] + F[Ped[k][1]]);

      for(j = rS; j; --j) { // trace back to reduced pedigree
        if(x[j]) {          // consider only ancestors of a father
          if(rPed[j][0]) x[rPed[j][0]] += x[j] * 0.5f;
          if(rPed[j][1]) x[rPed[j][1]] += x[j] * 0.5f;
          x[j] = B[j];
        }
      }

    }

    for(j = 1; j <= MIP; j++) // trace forth the reduced pedigree
      x[j] += (x[rPed[j][0]] + x[rPed[j][1]]) * 0.5f;

    for(i=0; i < *n; i++)      // obtain F for progeny of the current father
      if(S != Ped[SId[i]][0]) break;
      else F[SId[i]] = x[Link[Ped[SId[i]][1]]] * 0.5f;

    for(j = 1; j <= MIP; j++)
      x[j] = 0.0f;            // set to 0 for next evaluation of father
  }

}

void MyQuickSort(int Ped[][2], int *SId)
{
  return;
}

/*-------------------------------------------------------------------------
 * sargolzaei.c ends here */
