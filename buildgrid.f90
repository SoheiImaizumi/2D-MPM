module buildgrid_module

  implicit none
  integer, parameter :: dp2 = selected_real_kind(15, 307)

Contains
  subroutine bgrid(xI, h, nI_horiz, nI_vert, nI)

    integer :: k, r, q
    integer, intent(in) :: nI_horiz, nI_vert
    integer, intent(out) :: nI
    real(dp2) :: h
    real(dp2), allocatable :: xxI(:), xyI(:)
    real(dp2), allocatable, intent(inout) :: xI(:,:)

    nI = (nI_horiz +1) * (nI_vert +1)

    allocate(xxI(nI), xyI(nI))
    allocate(xI(2, nI))

    do q = 1, nI_horiz +1  
      do r = 1, nI_vert +1
        xxI((q-1) * (nI_vert+1) + r) = h * (q-1)
        xyI((q-1) * (nI_vert+1) + r) = h * (r-1)
      end do
    end do

    do k = 1, nI
      xI(1, k) = xxI(k)
      xI(2, k) = xyI(k) 
      !write(*,*) xI(:, k)
    end do

  end subroutine bgrid

end module buildgrid_module