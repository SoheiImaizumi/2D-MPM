module boundary_module
  
  implicit none
  integer, parameter :: dp7 = selected_real_kind(15, 307)

Contains
  subroutine bcondition(nI_horiz, fI, pI_new, nI)

    integer :: q
    integer, intent(in) :: nI_horiz, nI
    real(dp7), intent(inout) :: fI(2, nI), pI_new(2, nI)

    do q = 1, nI_horiz +1
      fI(2, q) = 0.0d0
      pI_new(2, q) = 0.0d0
    end do
  
  end subroutine
end module boundary_module