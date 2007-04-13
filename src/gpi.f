!     gpi.for
!-----------------------------------------------------------------------
!     What:  Genotype Probability Index (GPI) calculation
!     $Id: gpi.f 1153 2007-03-02 17:12:03Z ggorjan $
!     Time-stamp: <2007-03-02 18:02:17 ggorjan>
!     Subroutines: gpi, gpiCore
!-----------------------------------------------------------------------
!
!     Subroutine: gpi
!
!     Genotype Probability Index (GPI) for a set of individuals -
!     wrapper around subroutine gpiCore (look bellow)
!
!     Input:
!
!      nobs integer(1), number of individuals/records
!
!      n    integer(1), number of alleles
!
!      gp   double precision(k, nobs), individual genotype probabilities
!
!      hwp  double precision(k, nobs), Hardy-Weinberg genotype probabilities
!
!     Return:
!
!      ret  double precision(nobs), individual genotype probability indices
!
!     gp and hwp expect values for all possible genotypes, except for
!     the homozygote of the last allele as shown bellow. Note that
!     dimensions of hwp must be the same to dimensions of gp to allow
!     for possibly different values of hwp per individual. This is
!     handy, but hwp needs to properly prepaired - i.e. recycled - done
!     at R level.

      subroutine gpi(nobs, n, gp, hwp, ret)
        implicit none
        integer nobs, n, k, i, j
        double precision gp(n*(n+1)/2-1, nobs), hwp(n*(n+1)/2-1, nobs)
        double precision ret(nobs), gp2(n*(n+1)/2-1), hwp2(n*(n+1)/2-1)

        k=n*(n+1)/2-1

!       Loop over individuals i.e. calculate GPI for each individual
        do i = 1, nobs
!         Get gp2 and hwp2 for each individual
          do j = 1, k
            gp2(j) = gp(j, i)
            hwp2(j) = hwp(j, i)
          end do
          call gpiCore(n, k, gp2, hwp2, ret(i))
        end do
        return
      end subroutine

!-----------------------------------------------------------------------
!
!     Subroutine: gpiCore
!
!     Genotype Probability Index (GPI) for an individual
!
!     Method is described in Percy, A. and Kinghorn, B.P. 2005.
!     A genotype probability index for multiple alleles and haplotypes.
!     Journal of Animal Breeding and Genetics, 122: 387-392
!     http://dx.doi.org/10.1111/j.1439-0388.2005.00553.x
!
!     Gregor Gorjanc modified code by Percy and Kinghorn to FORTRAN 77
!     for maximal portability with R and added documentation.
!
!     Input:
!
!      n   integer(1), number of alleles
!
!      k   integer(1), number of dimensions for 'multiangles' = n*(n+1)/2-1
!
!      gp  double precison(k), individual's genotype probabilities
!
!      hwp double precison(k), Hardy-Weinberg genotype probabilities
!
!     Return:
!
!      ret double precision(1), individual's genotype probability index
!
!     gp and hwp expect values for all possible genotypes, except for the
!     homozygote of the last allele e.g. for
!
!     2 alleles: 1 and 2
!     11 12
!     --> k=2
!
!     3 alleles: 1, 2, and 3
!     11 12 13
!        22 23
!     --> k=5
!
!     4 alleles: 1, 2, 3, and 4
!     11 12 13 14
!        22 23 24
!           33 34
!     --> k=9
!
!     5 alleles: 1, 2, 3, 4, and 5
!     11 12 13 14 15
!        22 23 24 25
!           33 34 35
!              44 45
!     --> k=14

      subroutine gpiCore(n, k, gp, hwp, ret)
        implicit none
        integer n, ndim, i, j, k
        double precision a(k, k), b(k, 1), gp(k, 1), hwp(k, 1)
        double precision tmp(k, 1), c, qa, qb, qc, t, ret

        ndim = n*(n+1)/2-1
        do i = 1, ndim
          do j = 1, ndim
            a(i, j) = 0
          end do
        end do
        a(1, 1) = 1
        do i = 2, ndim
          a(i, i) = 0
          do j = 1, i-1
            a(i, i) = a(i, i) + ((a(j, j) / (float(j) + 1.))**2)
          end do
          a(i, i) = sqrt(1 - a(i, i))
        end do
        do i = 1, ndim
          b(i, 1) = (1. / (float(i) + 1.)) * a(i, i)
          do j = i+1, ndim
            a(i, j) = (1. / (float(i) + 1.)) * a(i, i)
          end do
        end do
        c = (float(ndim) + 1.) / (float(ndim) * a(ndim, ndim))

!        hwp = c * (matmul(a, hwp) - b)
        call ggmatmul(a, hwp, ndim, ndim, ndim, 1, tmp)
        do i = 1, ndim
          hwp(i, 1) = c * (tmp(i, 1) - b(i, 1))
        enddo

!        gp = c * (matmul(a, gp) - b)
        call ggmatmul(a, gp, ndim, ndim, ndim, 1, tmp)
        do i = 1, ndim
          gp(i, 1) = c * (tmp(i, 1) - b(i, 1))
        enddo

        qa = 0.
        qb = 0.
        qc = 0.
        do i = 1, ndim
          qa = qa +  (gp(i, 1) - hwp(i, 1))**2
          qb = qb + 2*hwp(i, 1) * (gp(i, 1) - hwp(i, 1))
          qc = qc +   hwp(i, 1) * hwp(i, 1)
        end do
        qc = qc-1
        if(qa <. 00000001) then
          ret = 0.
        else
          t = (-qb + sqrt(qb*qb - 4*qa*qc) ) / (2*qa)
          ret = 100 / t
        end if
        return
      end subroutine

!-----------------------------------------------------------------------
!     gpi.for ends here
