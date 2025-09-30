module I_renew_module ! over all nodes

  implicit none
  integer, parameter :: dp6 = selected_real_kind(15, 307)

Contains
  subroutine I_renew(nI, dt, pI, fI, pI_new) 

    integer :: l, j
    integer, intent(in) :: nI
    real(dp6), intent(in) :: dt, pI(2, nI), fI(2, nI)
    real(dp6), intent(out) :: pI_new(2, nI)

    do l = 1, nI
      do j = 1, 2
        pI_new(j, l) = pI(j, l) + fI(j, l) *dt
      end do
    end do

  end subroutine I_renew

end module I_renew_module