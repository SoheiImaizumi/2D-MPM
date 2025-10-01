module p2n_interpolation_module

  implicit none
  integer, parameter :: dp5 = selected_real_kind(15, 307)

Contains  
  subroutine p2n_interpolation(NIp, grad_NIp, mp, pp, volp, g, sigma, pI, fI, &
                               volI, mI, nI, np)                                 !every sigle node

    integer :: i, l, j, np, nI
    real(dp5), intent(in) :: NIp(2, nI, np), grad_NIp(2, nI, np), &
                            mp(np), pp(2, np), volp(np), g, sigma(3, np)
    real(dp5), intent(inout) :: pI(2, nI), fI(2, nI), volI(nI), mI(nI) 
    real(dp5) :: fIint(2, nI), fIext(2, nI)
    real(dp5) :: cutoff

    cutoff = 1.0e-12

    fIint = 0.0d0
    fIext = 0.0d0

    do l = 1, nI
      do i = 1, np
        do j = 1, 2
          mI(l) = mI(l) + (NIp(j,l,i) * NIp(3 - j,l,i)) * mp(i)
          volI(l) = volI(l) + (NIp(j,l,i) * NIp(3 - j,l,i)) * volp(i)

          fIint(j,l) = fIint(j,l) + volp(i) * (sigma(j,i)*grad_NIp(j,l,i) + sigma(3,i)*grad_NIp(3-j,l,i))
          pI(j,l) = pI(j,l) + NIp(j,l,i) * pp(j,i)
        end do
      end do

      fIext(1,l) = 0.0d0
      fIext(2,l) = - g * mI(l)
    
      do j = 1,2
        if (pI(j,l) < cutoff) then
          pI(j,l) = 0.0d0
        end if
        
        fI(j, l) = fIint(j, l) + fIext(j, l)
      end do

      !if (abs(fI(2, l)) >= 1.0d0) then
      !  write(*,*) fI(2, l)
      !else if (abs(fI(2, l)) < 1.0d0) then
      !  write(*,*) 'Chikkoiyo'
      !end if
    end do

  end subroutine p2n_interpolation

end module p2n_interpolation_module
  
  