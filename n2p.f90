module n2p_extrapolation_module ! using MUSL ! every single particle

  implicit none
  integer, parameter :: dp8 = selected_real_kind(15, 307)

Contains
  subroutine n2p_extrapolation(nI, dt, xp, mI, vp_new, NIp, pp, &
                               pp_new, vp, xp_new, pI_new, np, npm, fI)

    integer :: l, i, j
    integer, intent(in) :: nI, np, npm
    real(dp8), intent(in) :: dt, mI(nI), xp(2, np), NIp(2, nI, np), &
                            pI_new(2, nI), fI(2, nI), pp(2, np), vp(2, np)
    real(dp8), intent(out) :: vp_new(2, np), xp_new(2, np), pp_new(2, np)
    real(dp8) :: pinc(2, nI), vinc(2, nI), xinc(2, nI)

    do i = 1, npm
      pinc = 0.0d0
      vinc = 0.0d0
      xinc = 0.0d0

      pp_new(:, i) = 0.0d0

      do l = 1, nI
        do j = 1, 2
          if (mI(l) >= 1.0d-4) then
            vinc(j, l) = NIp(j, l, i) * fI(j, l) / (mI(l))
            xinc(j, l) = NIp(j, l, i) * pI_new(j, l) / (mI(l))
            pinc(j, l) = NIp(j, l, i) * pI_new(j, l)
          else
            vinc(j, l) = 0.0d0
            xinc(j, l) = 0.0d0
            pinc(j, l) = 0.0d0   
          end if
        end do

      end do

      do j = 1, 2
        xp_new(j, i) = xp(j, i) + dt * sum(xinc(j, :))
        pp_new(j, i) = pp(j, i) + dt * sum(pinc(j, :))
        vp_new(j, i) = vp(j, i) + dt * sum(vinc(j, :))
      end do
      
    end do

    do i = npm +1, np
      do j = 1, 2
        xp_new(j, i) = xp(j, i)
        pp_new(j, i) = 0.0d0
        vp_new(j, i) = 0.0d0
      end do  
    end do

  end subroutine n2p_extrapolation

end module n2p_extrapolation_module
