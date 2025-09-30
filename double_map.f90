module double_mapping_module

  implicit none
  integer, parameter :: dp9 = selected_real_kind(15, 307)

Contains  
  subroutine double_mapping(NIp, pp_new, mI, nI, np, vI_dm)                                 !every sigle node

    integer :: i, j, l
    integer, intent(in) :: np, nI
    real(dp9), intent(in) :: NIp(2, nI, np), pp_new(2, np), mI(nI)
    real(dp9), intent(out) :: vI_dm(2, nI)
    real(dp9) :: cutoff

    vI_dm = 0.0d0
    cutoff = 1.0e-6
 
    do l = 1, nI
      vI_dm(:, l) = 0.0d0 

      do i = 1, np
        do j = 1, 2
          if (mI(l) >= cutoff) then
            vI_dm(j, l) = vI_dm(j, l) + NIp(j, l, i) * pp_new(j, i) / (mI(l))
          else
            vI_dm(j, l) = 0.0d0
          end if
        end do
      end do
    end do

  end subroutine double_mapping

end module double_mapping_module
