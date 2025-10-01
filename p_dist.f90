module particle_distribution_module

  implicit none
  integer, parameter :: dp3 = selected_real_kind(15, 307)

Contains
  subroutine particle_distribution(M0, h, np, Vol, v0, xp, pp, vp, mp, volp0)

    integer :: i, j
    integer, intent(in) :: np
    real(dp3), intent(in) :: M0, Vol, h, v0(2)
    real(dp3), intent(out) :: xp(2, np), vp(2, np), mp(np), volp0(np), pp(2, np)

    xp(1,1) = 4* h - 2* h /3
    xp(1,2) = 4* h - h / 3
    xp(1,3) = 4* h + 2* h / 3
    xp(1,4) = 4* h + h / 3
    xp(1,5) = 4* h - 2* h /3
    xp(1,6) = 4* h - h / 3
    xp(1,7) = 4* h + 2* h / 3
    xp(1,8) = 4* h + h / 3
    xp(1,9) = 4* h - 2* h /3
    xp(1,10) = 4* h - h / 3
    xp(1,11) = 4* h + 2* h / 3
    xp(1,12) = 4* h + h / 3
    xp(1,13) = 4* h - 2* h /3
    xp(1,14) = 4* h - h / 3
    xp(1,15) = 4* h + 2* h / 3
    xp(1,16) = 4* h + h / 3

    xp(2,1) = 36* h - 2* h / 3
    xp(2,2) = 36* h - 2* h / 3
    xp(2,3) = 36* h - 2* h / 3
    xp(2,4) = 36* h - 2* h / 3
    xp(2,5) = 36* h - h / 3
    xp(2,6) = 36* h - h / 3
    xp(2,7) = 36* h - h / 3
    xp(2,8) = 36* h - h / 3
    xp(2,9) = 36* h + h / 3
    xp(2,10) = 36* h + h / 3
    xp(2,11) = 36* h + h / 3
    xp(2,12) = 36* h + h / 3
    xp(2,13) = 36* h + 2* h / 3
    xp(2,14) = 36* h + 2* h / 3
    xp(2,15) = 36* h + 2* h / 3
    xp(2,16) = 36* h + 2* h / 3


    do i = 1, np
      mp(i) = M0 / np
      volp0(i) = Vol / np
    end do

    do j = 1, 2
      do i = 1, np
        vp(j, i) = v0(j)
        pp(j, i) = mp(i) * vp(j, i)
      end do
    end do

  end subroutine particle_distribution

end module particle_distribution_module