!     ggmatmult.f
!-----------------------------------------------------------------------
!     What: Matrix multiplication, Gregor Gorjanc
!     $Id: ggmatmult.f 1153 2007-03-02 17:12:03Z ggorjan $
!     Time-stamp: <2007-03-02 17:56:51 ggorjan>
!-----------------------------------------------------------------------

      subroutine ggmatmul(x, y, xNrow, xNcol, yNrow, yNcol, z)
        implicit none
        integer xNrow, xNcol, yNrow, yNcol, i, j, k
        double precision x(xNrow, xNcol), y(yNrow, yNcol)
        double precision z(xNrow, yNcol)

        do i = 1, xNrow
          do j = 1, yNcol
            z(i, j) = .0d0
            do k = 1, xNcol
              z(i, j) = z(i, j) + x(i, k) * y(k, j)
!              print *, i, j, k, x(i, k), y(k, j), z(i, j)
            end do
          end do
        end do
        return
      end subroutine

!-----------------------------------------------------------------------
!     ggmatmult.for ends here
