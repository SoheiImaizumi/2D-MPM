module particle_distribution_module

  implicit none
  integer, parameter :: dp3 = selected_real_kind(15, 307)

Contains
  subroutine particle_distribution(M0, np, Vol, v0, xp, pp, vp, mp, volp0)

    integer :: i, j
    integer, intent(in) :: np
    real(dp3), intent(in) :: M0, Vol, v0(2)
    real(dp3), intent(out) :: xp(2, np), vp(2, np), mp(np), volp0(np), pp(2, np)

    xp(1,1) = 1.2e-1
    xp(1,2) = 1.4e-1
    xp(1,3) = 1.6e-1
    xp(1,4) = 1.8e-1
    xp(1,5) = 1.2e-1
    xp(1,6) = 1.4e-1
    xp(1,7) = 1.6e-1
    xp(1,8) = 1.8e-1
    xp(1,9) = 1.2e-1
    xp(1,10) = 1.4e-1
    xp(1,11) = 1.6e-1
    xp(1,12) = 1.8e-1
    xp(1,13) = 1.2e-1
    xp(1,14) = 1.4e-1
    xp(1,15) = 1.6e-1
    xp(1,16) = 1.8e-1

    xp(2,1) = 1.98
    xp(2,2) = 1.98
    xp(2,3) = 1.98
    xp(2,4) = 1.98
    xp(2,5) = 1.99
    xp(2,6) = 1.99
    xp(2,7) = 1.99
    xp(2,8) = 1.99
    xp(2,9) = 2.00
    xp(2,10) = 2.00
    xp(2,11) = 2.00
    xp(2,12) = 2.00
    xp(2,13) = 2.01
    xp(2,14) = 2.01
    xp(2,15) = 2.01
    xp(2,16) = 2.01

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